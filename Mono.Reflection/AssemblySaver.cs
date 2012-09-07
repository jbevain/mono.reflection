using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

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

			foreach (var nested_type in type.GetNestedTypes (BindingFlags.Public | BindingFlags.NonPublic))
				MapType (nested_type, type_definition);

			MapCustomAttributes (type, type_definition);
		}

		private void MapMethod (TypeDefinition type_definition, MethodBase method)
		{
			var method_definition = MethodDefinitionFor (method, type_definition);

			MapCustomAttributes (method, method_definition);

			if (!method_definition.HasBody)
				return;

			MapMethodBody (method, method_definition);
		}

		private void MapField (TypeDefinition type_definition, FieldInfo field)
		{
			var field_definition = FieldDefinitionFor (field, type_definition);

			MapCustomAttributes (field, field_definition);
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
					il.Emit (op, CreateReference ((MethodBase) instruction.Operand));
					break;
				case OperandType.InlineField:
					il.Emit (op, CreateReference ((FieldInfo) instruction.Operand));
					break;
				case OperandType.InlineTok:
					var member = (MemberInfo) instruction.Operand;
					if (member is Type)
						il.Emit (op, CreateReference ((Type) instruction.Operand));
					else if (member is FieldInfo)
						il.Emit (op, CreateReference ((FieldInfo) instruction.Operand));
					else if (member is MethodBase)
						il.Emit (op, CreateReference ((MethodBase) instruction.Operand));
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
				var handler = new ExceptionHandler ((ExceptionHandlerType) clause.Flags);

				handler.TryStart = OffsetToInstruction (clause.TryOffset, instructions, method_definition);
				handler.TryEnd = OffsetToInstruction (clause.TryOffset + clause.TryLength, instructions, method_definition);
				handler.HandlerStart = OffsetToInstruction (clause.HandlerOffset, instructions, method_definition);
				handler.HandlerEnd = OffsetToInstruction (clause.HandlerOffset + clause.HandlerLength, instructions, method_definition);

				switch (handler.HandlerType) {
				case ExceptionHandlerType.Catch:
					handler.CatchType = CreateReference (clause.CatchType, method_definition);
					break;
				case ExceptionHandlerType.Filter:
					handler.FilterStart = OffsetToInstruction (clause.FilterOffset, instructions, method_definition);
					break;
				}
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

			var method_info = method as MethodInfo;

			if (method_info != null)
				foreach (var arg in method.GetGenericArguments ())
					method_definition.GenericParameters.Add (new GenericParameter (arg.Name, method_definition));

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

			foreach (var arg in type.GetGenericArguments ())
				type_definition.GenericParameters.Add (new GenericParameter (arg.Name, type_definition));

			if (declaringType == null)
				_assembly_definition.MainModule.Types.Add (type_definition);
			else
				declaringType.NestedTypes.Add (type_definition);

			type_definition.BaseType = CreateReference (type.BaseType, type_definition);

			return type_definition;
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

		private static OpCode OpCodeFor (Instruction instruction)
		{
			return typeof (OpCodes)
				.GetFields (BindingFlags.Static | BindingFlags.Public)
				.Select (f => f.GetValue (null))
				.Cast<OpCode> ()
				.First (o => o.Name == instruction.OpCode.Name);
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

		private FieldReference CreateReference (FieldInfo field)
		{
			var reference = _module_definition.Import (field);
			MapReference (reference.DeclaringType);
			return reference;
		}

		private MethodReference CreateReference (MethodBase method)
		{
			var reference = _module_definition.Import (method);
			MapReference (reference.GetElementMethod ().DeclaringType);
			return reference;
		}

		private TypeReference MapReference (TypeReference type)
		{
			if (type.Scope.MetadataScopeType != MetadataScopeType.AssemblyNameReference)
				return type;

			var reference = (AssemblyNameReference) type.Scope;
			if (reference.FullName != _assembly_definition.FullName)
				return type;

			type.GetElementType ().Scope = _module_definition;
			_module_definition.AssemblyReferences.Remove (reference);
			return type;
		}

		private void MapCustomAttributes (SR.ICustomAttributeProvider provider, Cecil.ICustomAttributeProvider targetProvider)
		{
			var method = provider.GetType ().GetMethod ("GetCustomAttributesData");
			if (method == null)
				throw new NotSupportedException ("No method GetCustomAttributesData for type " + provider.GetType ().FullName);

			var custom_attributes_data = (IList<CustomAttributeData>) method.Invoke (provider, new object[0]);

			foreach (var custom_attribute_data in custom_attributes_data) {
				var custom_attribute = new CustomAttribute (CreateReference (custom_attribute_data.Constructor));

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
			return new CustomAttributeArgument (CreateReference (argument.ArgumentType), argument.Value);
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
