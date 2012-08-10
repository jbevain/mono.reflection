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

				foreach (var property in type.GetProperties(AllDeclared)) {
					var property_definition = PropertyDefinitionFor (property, type_definition);

					MapProperty (property, property_definition);
				}
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
				_module_definition.Import (property.PropertyType, declaringType));

			declaringType.Properties.Add (property_definition);

			return property_definition;
		}

		private void MapMethodBody (MethodBase method, MethodDefinition method_definition)
		{
			foreach (var instruction in method.GetInstructions ()) {

				MapVariables (method, method_definition);

				var il = method_definition.Body.GetILProcessor ();

				var op = OpCodeFor (instruction);

				switch (op.OperandType) {
				case OperandType.InlineNone:
					il.Emit (op);
					break;
				case OperandType.InlineMethod:
					il.Emit (op, _module_definition.Import ((MethodBase) instruction.Operand));
					break;
				case OperandType.InlineField:
					il.Emit (op, _module_definition.Import ((FieldInfo) instruction.Operand));
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
				var variable_type = _module_definition.Import (variable.LocalType);
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
					_module_definition.Import (parameter.ParameterType, method_definition)));

			if (method_info != null)
				method_definition.ReturnType = _module_definition.Import (method_info.ReturnType, method_definition);

			return method_definition;
		}

		private FieldDefinition FieldDefinitionFor (FieldInfo field, TypeDefinition declaringType)
		{
			var field_definition = new FieldDefinition (
				field.Name,
				(Cecil.FieldAttributes) field.Attributes,
				_assembly_definition.MainModule.Import (field.FieldType, declaringType));

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

			type_definition.BaseType = _assembly_definition.MainModule.Import (type.BaseType, type_definition);

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
