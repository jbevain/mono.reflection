using System;
using System.IO;
using System.Linq;
using System.Reflection;

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

			foreach (var type in _assembly.GetTypes().Where(t => !t.IsNested)) {
				var type_definition = TypeDefinitionFor(type);

				foreach (var field in type.GetFields(AllDeclared))
					FieldDefinitionFor (field, type_definition);

				foreach (var method in type.GetConstructors (AllDeclared).Cast<MethodBase> ().Concat (type.GetMethods (AllDeclared))) {
					var method_definition = MethodDefinitionFor (method, type_definition);
					if (!method_definition.HasBody)
						continue;

					MapMethodBody (method, method_definition);
				}

				foreach (var property in type.GetProperties (AllDeclared))
					MapProperty (property, PropertyDefinitionFor (property, type_definition));

				foreach (var evt in type.GetEvents (AllDeclared))
					MapEvent (evt, EventDefinitionFor (evt, type_definition));
			}

			return _assembly_definition;
		}

		private static void MapProperty (PropertyInfo property, PropertyDefinition property_definition)
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

		private static void MapEvent (EventInfo evt, EventDefinition event_definition)
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
		}

		private void MapInstructions (MethodBase method, MethodDefinition method_definition)
		{
			foreach (var instruction in method.GetInstructions ()) {
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
				case OperandType.InlineI:
					il.Emit (op, (int) instruction.Operand);
					break;
				case OperandType.InlineVar:
					il.Emit (op, VariableFor (instruction, method_definition));
					break;
				case OperandType.InlineArg:
					il.Emit (op, ParameterFor (instruction, method_definition));
					break;
				case OperandType.InlineString:
					il.Emit (op, (string) instruction.Operand);
					break;
				default:
					throw new NotImplementedException (op.OperandType.ToString ());
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
				method_definition.Parameters.Add (new ParameterDefinition (
					parameter.Name,
					(Cecil.ParameterAttributes) parameter.Attributes,
					CreateReference (parameter.ParameterType, method_definition)));

			if (method_info != null)
				method_definition.ReturnType = CreateReference (method_info.ReturnType, method_definition);

			return method_definition;
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

		private TypeDefinition TypeDefinitionFor (Type type)
		{
			var type_definition = new TypeDefinition (
				type.Namespace,
				type.Name,
				(Cecil.TypeAttributes) type.Attributes,
				_assembly_definition.MainModule.TypeSystem.Object);

			foreach (var arg in type.GetGenericArguments ())
				type_definition.GenericParameters.Add (new GenericParameter (arg.Name, type_definition));

			_assembly_definition.MainModule.Types.Add (type_definition);

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
				.Select (f => f.GetValue(null))
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
			reference.DeclaringType = MapReference (reference.DeclaringType);
			return reference;
		}

		private MethodReference CreateReference (MethodBase method)
		{
			var reference = _module_definition.Import (method);
			reference.DeclaringType = MapReference (reference.DeclaringType);
			return reference;
		}

		private TypeReference MapReference (TypeReference type)
		{
			if (type.Scope.MetadataScopeType != MetadataScopeType.AssemblyNameReference)
				return type;

			var reference = (AssemblyNameReference) type.Scope;
			if (reference.FullName != _assembly_definition.FullName)
				return type;

			type.Scope = _module_definition;
			_module_definition.AssemblyReferences.Remove (reference);
			return type;
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
