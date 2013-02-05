using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using SR = System.Reflection;

using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Mono.Reflection {

	class MetadataMapper {

		private const BindingFlags AllDeclared = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly;

		public static AssemblyDefinition MapAssembly(Assembly assembly)
		{
			var mapper = new MetadataMapper (assembly);
			return mapper.Map ();
		}

		private readonly Assembly _assembly;

		private AssemblyDefinition _assembly_definition;
		private ModuleDefinition _module_definition;

		private MetadataMapper (Assembly assembly)
		{
			_assembly = assembly;
		}

		private AssemblyDefinition Map ()
		{
			_assembly_definition = AssemblyDefinitionFor (_assembly);
			_module_definition = _assembly_definition.MainModule;

			foreach (var type in _assembly.GetTypes ().Where (t => !t.IsNested))
				MapType (type);

			MapCustomAttributes (_assembly, _assembly_definition);
			MapCustomAttributes (_assembly.ManifestModule, _assembly_definition.MainModule);

			return _assembly_definition;
		}

		private void MapType (Type type, TypeDefinition declaringType = null)
		{
			var type_definition = TypeDefinitionFor (type, declaringType);

			foreach (var field in type.GetFields (AllDeclared))
				MapField (type_definition, field);

			foreach (var method in type.GetConstructors (AllDeclared).Cast<MethodBase> ().Concat (type.GetMethods (AllDeclared)))
				MapMethod (type_definition, method);

			foreach (var property in type.GetProperties (AllDeclared))
				MapProperty (property, PropertyDefinitionFor (property, type_definition));

			foreach (var evt in type.GetEvents (AllDeclared))
				MapEvent (evt, EventDefinitionFor (evt, type_definition));

			foreach (var iface in type.GetInterfaces())
				type_definition.Interfaces.Add (CreateReference (iface, type_definition));

			foreach (var nested_type in type.GetNestedTypes (BindingFlags.Public | BindingFlags.NonPublic))
				MapType (nested_type, type_definition);

			MapCustomAttributes (type, type_definition);
		}

		private void MapMethod (TypeDefinition type_definition, MethodBase method)
		{
			var method_definition = MethodDefinitionFor (method, type_definition);

			MapCustomAttributes (method, method_definition);
			MapOverrides (method, method_definition);

			if (!ShouldMapBody(method, method_definition))
				return;

			MapMethodBody (method, method_definition);
		}

		private static bool ShouldMapBody (MethodBase method, MethodDefinition method_definition)
		{
			return method_definition.HasBody && method.GetMethodBody() != null;
		}

		private void MapOverrides (MethodBase method, MethodDefinition method_definition)
		{
			var mi = method as MethodInfo;
			if (mi == null || !mi.IsVirtual)
				return;

			var type = method.DeclaringType;
			if (type == null)
				return;

			var overrides = type
				.GetInterfaces ()
				.Select (type.GetInterfaceMap)
				.SelectMany (m => m.InterfaceMethods.Zip (m.TargetMethods, (im, tm) => new { InterfaceMethod = im, TargetMethod = tm }))
				.Where (p => p.TargetMethod.DeclaringType == type)
				.Where (p => p.InterfaceMethod.Name != p.TargetMethod.Name)
				.Where (p => p.TargetMethod == method)
				.Select (p => p.InterfaceMethod);

			foreach (var ov in overrides)
				method_definition.Overrides.Add (CreateReference (ov, method_definition).GetElementMethod ());
		}

		private void MapField (TypeDefinition type_definition, FieldInfo field)
		{
			var field_definition = FieldDefinitionFor (field, type_definition);

			if (field_definition.HasDefault)
				field_definition.Constant = field.GetRawConstantValue ();

			if ((field_definition.Attributes & Cecil.FieldAttributes.HasFieldRVA) != 0)
				field_definition.InitialValue = GetInitialValue (field);

			MapCustomAttributes (field, field_definition);
		}

		private static byte [] GetInitialValue (FieldInfo field)
		{
			if (!field.IsStatic)
				throw new NotSupportedException ();

			var value = field.GetValue (null);
			if (value == null)
				return new byte [0];

			var type = value.GetType ();
			if (!type.IsValueType || type.IsPrimitive)
				throw new NotSupportedException ();

			return ToByteArray (value);
		}

		private static byte [] ToByteArray (object @struct)
		{
			var size = Marshal.SizeOf (@struct.GetType ());
			var data = new byte [size];
			var ptr = Marshal.AllocHGlobal (size);

			Marshal.StructureToPtr (@struct, ptr, fDeleteOld: true);
			Marshal.Copy (ptr, data, 0, size);
			Marshal.FreeHGlobal (ptr);

			return data;
		}

		private void MapProperty (PropertyInfo property, PropertyDefinition property_definition)
		{
			var type_definition = property_definition.DeclaringType;

			var getter = property.GetGetMethod (nonPublic: true);
			if (getter != null) {
				property_definition.GetMethod = type_definition.Methods.Single (m => m.Name == getter.Name);
				property_definition.GetMethod.IsGetter = true;
			}

			var setter = property.GetSetMethod (nonPublic: true);
			if (setter != null) {
				property_definition.SetMethod = type_definition.Methods.Single (m => m.Name == setter.Name);
				property_definition.SetMethod.IsSetter = true;
			}

			MapCustomAttributes (property, property_definition);
		}

		private PropertyDefinition PropertyDefinitionFor (PropertyInfo property, TypeDefinition declaringType)
		{
			var property_definition = new PropertyDefinition (
				property.Name,
				(Cecil.PropertyAttributes) property.Attributes,
				CreateReference (property.PropertyType, declaringType));

			declaringType.Properties.Add (property_definition);

			return property_definition;
		}

		private void MapEvent (EventInfo evt, EventDefinition event_definition)
		{
			var type_definition = event_definition.DeclaringType;

			var add = evt.GetAddMethod (nonPublic: true);
			if (add != null) {
				event_definition.AddMethod = type_definition.Methods.Single (m => m.Name == add.Name);
				event_definition.AddMethod.IsAddOn = true;
			}

			var remove = evt.GetRemoveMethod (nonPublic: true);
			if (remove != null) {
				event_definition.RemoveMethod = type_definition.Methods.Single (m => m.Name == remove.Name);
				event_definition.RemoveMethod.IsRemoveOn = true;
			}

			var raise = evt.GetRaiseMethod (nonPublic: true);
			if (raise != null) {
				event_definition.InvokeMethod = type_definition.Methods.Single (m => m.Name == raise.Name);
				event_definition.InvokeMethod.IsFire = true;
			}

			MapCustomAttributes (evt, event_definition);
		}

		private EventDefinition EventDefinitionFor (EventInfo evt, TypeDefinition declaringType)
		{
			var event_definition = new EventDefinition (
				evt.Name,
				(Cecil.EventAttributes) evt.Attributes,
				CreateReference (evt.EventHandlerType, declaringType));

			declaringType.Events.Add (event_definition);

			return event_definition;
		}

		private void MapMethodBody (MethodBase method, MethodDefinition method_definition)
		{
			MapVariables (method, method_definition);
			MapInstructions (method, method_definition);
			MapExceptions (method, method_definition);
		}

		private void MapInstructions (MethodBase method, MethodDefinition method_definition)
		{
			var instructions = method.GetInstructions ();

			foreach (var instruction in instructions) {
				var il = method_definition.Body.GetILProcessor ();

				var op = OpCodeFor (instruction);

				switch (op.OperandType) {
				case OperandType.InlineNone:
					il.Emit (op);
					break;
				case OperandType.InlineMethod:
					il.Emit (op, CreateReference ((MethodBase) instruction.Operand, method_definition));
					break;
				case OperandType.InlineField:
					il.Emit (op, CreateReference ((FieldInfo) instruction.Operand, method_definition));
					break;
				case OperandType.InlineType:
					il.Emit (op, CreateReference ((Type) instruction.Operand, method_definition));
					break;
				case OperandType.InlineTok:
					var member = (MemberInfo) instruction.Operand;
					if (member is Type)
						il.Emit (op, CreateReference ((Type) instruction.Operand, method_definition));
					else if (member is FieldInfo)
						il.Emit (op, CreateReference ((FieldInfo) instruction.Operand, method_definition));
					else if (member is MethodBase)
						il.Emit (op, CreateReference ((MethodBase) instruction.Operand, method_definition));
					else
						throw new NotSupportedException ();
					break;
				case OperandType.ShortInlineI:
					if (op.Code == Code.Ldc_I4_S)
						il.Emit (op, (sbyte) instruction.Operand);
					else
						il.Emit (op, (byte) instruction.Operand);
					break;
				case OperandType.InlineI:
					il.Emit (op, (int) instruction.Operand);
					break;
				case OperandType.InlineI8:
					il.Emit (op, (long) instruction.Operand);
					break;
				case OperandType.ShortInlineR:
					il.Emit (op, (float) instruction.Operand);
					break;
				case OperandType.InlineR:
					il.Emit (op, (double) instruction.Operand);
					break;
				case OperandType.ShortInlineVar:
				case OperandType.InlineVar:
					il.Emit (op, VariableFor (instruction, method_definition));
					break;
				case OperandType.ShortInlineArg:
				case OperandType.InlineArg:
					il.Emit (op, ParameterFor (instruction, method_definition));
					break;
				case OperandType.InlineString:
					il.Emit (op, (string) instruction.Operand);
					break;
				case OperandType.ShortInlineBrTarget:
				case OperandType.InlineBrTarget:
					il.Emit (op, Cecil.Cil.Instruction.Create (OpCodes.Nop));
					break;
				case OperandType.InlineSwitch:
					il.Emit (op, new [] { Cecil.Cil.Instruction.Create (OpCodes.Nop) });
					break;
				case OperandType.InlineSig:
					throw new NotSupportedException ("InlineSig");
				default:
					throw new NotSupportedException (op.OperandType.ToString ());
				}
			}

			foreach (var instruction in instructions) {
				var op = OpCodeFor (instruction);

				switch (op.OperandType) {
				case OperandType.ShortInlineBrTarget:
				case OperandType.InlineBrTarget:
					var br = OffsetToInstruction (instruction.Offset, instructions, method_definition);
					var target = (Instruction) instruction.Operand;
					if (target != null)
						br.Operand = OffsetToInstruction (target.Offset, instructions, method_definition);

					break;

				case OperandType.InlineSwitch:
					var @switch = OffsetToInstruction (instruction.Offset, instructions, method_definition);
					@switch.Operand = ((Instruction []) instruction.Operand).Select (i => OffsetToInstruction (i.Offset, instructions, method_definition)).ToArray ();
					break;
				}
			}
		}

		private void MapVariables (MethodBase method, MethodDefinition method_definition)
		{
			var body = method.GetMethodBody ();
			if (body == null)
				return;

			foreach (var variable in body.LocalVariables) {
				var variable_type = CreateReference (variable.LocalType, method_definition);
				method_definition.Body.Variables.Add (new VariableDefinition (variable.IsPinned ? new PinnedType (variable_type) : variable_type));
			}

			method_definition.Body.InitLocals = body.InitLocals;
		}

		private void MapExceptions (MethodBase method, MethodDefinition method_definition)
		{
			var body = method.GetMethodBody ();
			if (body == null)
				return;

			var instructions = method.GetInstructions ();

			foreach (var clause in body.ExceptionHandlingClauses) {
				var handler = new ExceptionHandler ((ExceptionHandlerType) clause.Flags) {
					TryStart = OffsetToInstruction (clause.TryOffset, instructions, method_definition),
					TryEnd = OffsetToInstruction (clause.TryOffset + clause.TryLength, instructions, method_definition),
					HandlerStart = OffsetToInstruction (clause.HandlerOffset, instructions, method_definition),
					HandlerEnd = OffsetToInstruction (clause.HandlerOffset + clause.HandlerLength, instructions, method_definition)
				};

				switch (handler.HandlerType) {
				case ExceptionHandlerType.Catch:
					handler.CatchType = CreateReference (clause.CatchType, method_definition);
					break;
				case ExceptionHandlerType.Filter:
					handler.FilterStart = OffsetToInstruction (clause.FilterOffset, instructions, method_definition);
					break;
				}

				method_definition.Body.ExceptionHandlers.Add (handler);
			}
		}

		private static Cecil.Cil.Instruction OffsetToInstruction (int offset, IList<Instruction> instructions, MethodDefinition method_definition)
		{
			var instruction = instructions.FirstOrDefault (i => i.Offset == offset);
			if (instruction == null)
				return null;

			return method_definition.Body.Instructions [instructions.IndexOf (instruction)];
		}

		private static AssemblyDefinition AssemblyDefinitionFor (Assembly assembly)
		{
			var name = assembly.GetName ();

			var assembly_definition = AssemblyDefinition.CreateAssembly (
				new AssemblyNameDefinition (name.Name, name.Version),
				assembly.ManifestModule.Name, ModuleKind.Dll);

			assembly_definition.MainModule.Runtime = TargetRuntime.Net_4_0;
			return assembly_definition;
		}

		private MethodDefinition MethodDefinitionFor (MethodBase method, TypeDefinition declaringType)
		{
			var method_definition = new MethodDefinition (
				method.Name,
				(Cecil.MethodAttributes) method.Attributes,
				_module_definition.TypeSystem.Void);

			method_definition.ImplAttributes = (Cecil.MethodImplAttributes) (int) method.GetMethodImplementationFlags ();

			var method_info = method as MethodInfo;

			if (method_info != null)
				foreach (var generic_parameter in MapGenericParameters (method_info.GetGenericArguments (), method_definition))
					method_definition.GenericParameters.Add (generic_parameter);

			declaringType.Methods.Add (method_definition);

			foreach (var parameter in method.GetParameters ())
				MapParameter (method_definition, parameter);

			if (method_info != null)
				method_definition.ReturnType = CreateReference (method_info.ReturnType, method_definition);

			return method_definition;
		}

		private void MapParameter (MethodDefinition method_definition, ParameterInfo parameter)
		{
			var parameter_definition = new ParameterDefinition (
				parameter.Name,
				(Cecil.ParameterAttributes) parameter.Attributes,
				CreateReference (parameter.ParameterType, method_definition));

			MapCustomAttributes (parameter, parameter_definition);

			method_definition.Parameters.Add (parameter_definition);
		}

		private FieldDefinition FieldDefinitionFor (FieldInfo field, TypeDefinition declaringType)
		{
			var field_definition = new FieldDefinition (
				field.Name,
				(Cecil.FieldAttributes) field.Attributes,
				CreateReference (field.FieldType, declaringType));

			declaringType.Fields.Add (field_definition);

			return field_definition;
		}

		private TypeDefinition TypeDefinitionFor (Type type, TypeDefinition declaringType)
		{
			var type_definition = new TypeDefinition (
				type.Namespace,
				type.Name,
				(Cecil.TypeAttributes) type.Attributes,
				_assembly_definition.MainModule.TypeSystem.Object);

			foreach (var generic_parameter in MapGenericParameters (type.GetGenericArguments (), type_definition))
				type_definition.GenericParameters.Add (generic_parameter);

			if (declaringType == null)
				_assembly_definition.MainModule.Types.Add (type_definition);
			else
				declaringType.NestedTypes.Add (type_definition);

			type_definition.BaseType = CreateReference (type.BaseType, type_definition);

			var layout = type.StructLayoutAttribute;

			if (layout != null) {
				type_definition.PackingSize = (short) layout.Pack;
				type_definition.ClassSize = layout.Size;
			}

			return type_definition;
		}

		private IEnumerable<GenericParameter> MapGenericParameters (Type [] genericParameters, IGenericParameterProvider owner)
		{
			foreach (var p in genericParameters) {
				var generic_parameter = new GenericParameter (p.Name, owner) {
					Attributes = (Cecil.GenericParameterAttributes) (int) p.GenericParameterAttributes
				};

				foreach (var constraint in p.GetGenericParameterConstraints ())
					generic_parameter.Constraints.Add (CreateReference (constraint));

				yield return generic_parameter;
			}
		}

		private static ParameterDefinition ParameterFor (Instruction instruction, MethodDefinition method)
		{
			var parameter = (ParameterInfo) instruction.Operand;
			return method.Parameters [parameter.Position];
		}

		private static VariableDefinition VariableFor (Instruction instruction, MethodDefinition method)
		{
			var local = (LocalVariableInfo) instruction.Operand;
			return method.Body.Variables [local.LocalIndex];
		}

		private static readonly OpCode[] _opcodes = typeof (OpCodes)
			.GetFields (BindingFlags.Static | BindingFlags.Public)
			.Select (f => f.GetValue (null))
			.Cast<OpCode> ()
			.ToArray ();

		private static OpCode OpCodeFor (Instruction instruction)
		{
			foreach (var opcode in _opcodes)
				if (opcode.Value == instruction.OpCode.Value)
					return opcode;

			throw new NotSupportedException("OpCode not found: " + instruction.OpCode.Name);
		}

		private TypeReference CreateReference (Type type)
		{
			return MapReference (_module_definition.Import (type));
		}

		private TypeReference CreateReference (Type type, TypeReference context)
		{
			return MapReference (_module_definition.Import (type, context));
		}

		private TypeReference CreateReference (Type type, MethodReference context)
		{
			return MapReference (_module_definition.Import (type, context));
		}

		private FieldReference CreateReference (FieldInfo field, MethodReference context)
		{
			var reference = _module_definition.Import (field, context);
			MapReference (reference.DeclaringType);
			MapReference (reference.FieldType);
			return reference;
		}

		private MethodReference CreateReference (MethodBase method, MethodReference context)
		{
			var reference = _module_definition.Import (method, context);
			MapReference (reference.GetElementMethod ().DeclaringType);
			MapReference (reference.ReturnType);
			MapGenericArguments (reference);

			foreach (var parameter in reference.Parameters)
				MapReference (parameter.ParameterType);

			return reference;
		}

		private void MapGenericArguments (MemberReference reference)
		{
			var instance = reference as IGenericInstance;
			if (instance == null)
				return;

			foreach (var arg in instance.GenericArguments)
				MapReference (arg);
		}

		private TypeReference MapReference (TypeReference type)
		{
			if (type.IsGenericParameter)
				return type;

			if (type.IsPointer || type.IsByReference || type.IsPinned || type.IsArray) {
				MapElementType (type);
				return type;
			}

			if (type.IsGenericInstance) {
				MapGenericArguments (type);
				MapElementType (type);
				return type;
			}

			if (type.Scope.MetadataScopeType != MetadataScopeType.AssemblyNameReference)
				return type;

			var reference = (AssemblyNameReference) type.Scope;
			if (reference.FullName != _assembly_definition.FullName)
				return type;

			type.GetElementType ().Scope = _module_definition;
			_module_definition.AssemblyReferences.Remove (reference);
			return type;
		}

		private void MapElementType (TypeReference type)
		{
			MapReference (((TypeSpecification) type).ElementType);
		}

		private void MapCustomAttributes (SR.ICustomAttributeProvider provider, Cecil.ICustomAttributeProvider targetProvider)
		{
			var method = provider.GetType ().GetMethod ("GetCustomAttributesData");
			if (method == null)
				throw new NotSupportedException ("No method GetCustomAttributesData for type " + provider.GetType ().FullName);

			var custom_attributes_data = (IList<CustomAttributeData>) method.Invoke (provider, new object[0]);

			foreach (var custom_attribute_data in custom_attributes_data) {
				var custom_attribute = new CustomAttribute (CreateReference (custom_attribute_data.Constructor, null));

				foreach (var argument in custom_attribute_data.ConstructorArguments) {
					custom_attribute.ConstructorArguments.Add (CustomAttributeArgumentFor (argument));
				}

				foreach (var named_argument in custom_attribute_data.NamedArguments) {
					var argument = new Cecil.CustomAttributeNamedArgument (named_argument.MemberInfo.Name, CustomAttributeArgumentFor (named_argument.TypedValue));
					if (named_argument.MemberInfo is PropertyInfo)
						custom_attribute.Properties.Add (argument);
					else if (named_argument.MemberInfo is FieldInfo)
						custom_attribute.Fields.Add (argument);
					else
						throw new NotSupportedException ();
				}

				targetProvider.CustomAttributes.Add (custom_attribute);
			}
		}

		private CustomAttributeArgument CustomAttributeArgumentFor (CustomAttributeTypedArgument argument)
		{
			return new CustomAttributeArgument (
				CreateReference (argument.ArgumentType),
				MapCustomAttributeValue (argument));
		}

		private object MapCustomAttributeValue (CustomAttributeTypedArgument argument)
		{
			var value = argument.Value;
			var type = value as Type;
			if (type != null)
				return CreateReference (type);

			return value;
		}
	}

	public static class AssemblySaver {

		public static void SaveTo (this Assembly assembly, string fileName)
		{
			using (var stream = new FileStream (fileName, FileMode.Create, FileAccess.ReadWrite))
				assembly.SaveTo (stream);
		}

		public static void SaveTo (this Assembly assembly, Stream stream)
		{
			if (assembly == null)
				throw new ArgumentNullException ("assembly");

			var assembly_definition = MetadataMapper.MapAssembly (assembly);
			assembly_definition.Write (stream);
		}
	}
}
