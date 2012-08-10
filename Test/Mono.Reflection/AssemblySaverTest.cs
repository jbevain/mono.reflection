using System;
using System.IO;
using System.Linq;

using SR = System.Reflection;
using SRE = System.Reflection.Emit;

using Mono.Cecil;
using Mono.Cecil.Cil;

using NUnit.Framework;
using MethodAttributes = System.Reflection.MethodAttributes;
using PropertyAttributes = System.Reflection.PropertyAttributes;

namespace Mono.Reflection {

	[TestFixture]
	public class AssemblySaverTest : BaseReflectionTest {

		private static ModuleDefinition Save (Action<SRE.AssemblyBuilder, SRE.ModuleBuilder> definer)
		{
			var name = "Save-" + Guid.NewGuid ().ToString ("D");
			var assembly = AppDomain.CurrentDomain.DefineDynamicAssembly (new SR.AssemblyName (name), SRE.AssemblyBuilderAccess.Run);
			var module = assembly.DefineDynamicModule (name);

			definer (assembly, module);

			var memory = new MemoryStream ();
			assembly.SaveTo (memory);

			return ModuleDefinition.ReadModule (new MemoryStream (memory.ToArray ()));
		}

		[Test]
		public void SimpleType ()
		{
			var module = Save ((a, m) => m.DefineType ("Foo.Bar").CreateType ());

			var type = module.GetType ("Foo.Bar");
			Assert.IsNotNull (type);
		}

		[Test]
		public void SimpleMethod ()
		{
			var module = Save ((a, m) => {
				var type = m.DefineType ("Foo.Bar");

				var method = type.DefineMethod ("Baz", SR.MethodAttributes.Public | SR.MethodAttributes.Static, typeof (object), new [] { typeof (object) });
				var il = method.GetILGenerator ();
				il.Emit (SRE.OpCodes.Ldarg_0);
				il.Emit (SRE.OpCodes.Ret);

				type.CreateType ();
			});

			var bar = module.GetType ("Foo.Bar");
			var baz = bar.Methods.Single (m => m.Name == "Baz");

			Assert.AreEqual (2, baz.Body.Instructions.Count);
			Assert.AreEqual (OpCodes.Ldarg_0, baz.Body.Instructions [0].OpCode);
			Assert.AreEqual (OpCodes.Ret, baz.Body.Instructions [1].OpCode);
		}

		[Test]
		public void MethodWithVariable ()
		{
			var module = Save ((a, m) => {
				var type = m.DefineType ("Foo.Bar");

				var method = type.DefineMethod ("Baz", SR.MethodAttributes.Public | SR.MethodAttributes.Static, typeof (object), new [] { typeof (object) });
				var il = method.GetILGenerator ();
				var o = il.DeclareLocal (typeof (object));
				il.Emit (SRE.OpCodes.Ldarg_0);
				il.Emit (SRE.OpCodes.Stloc, o);
				il.Emit (SRE.OpCodes.Ldloc, o);
				il.Emit (SRE.OpCodes.Ret);

				type.CreateType ();
			});

			var bar = module.GetType ("Foo.Bar");
			var baz = bar.Methods.Single (m => m.Name == "Baz");

			Assert.AreEqual (4, baz.Body.Instructions.Count);
		}

		[Test]
		public void SimpleProperty ()
		{
			var module = Save ((a, m) => {
				var type = m.DefineType ("Foo.Bar");

				var property = type.DefineProperty ("Baz", PropertyAttributes.None, typeof (int), Type.EmptyTypes);

				var set_Baz = type.DefineMethod ("set_Baz", MethodAttributes.Public, typeof (void), new[] { typeof (int) });
				set_Baz.GetILGenerator().Emit (SRE.OpCodes.Ret);

				var get_Baz = type.DefineMethod ("get_Baz", MethodAttributes.Public, typeof (int), Type.EmptyTypes);
				var il = get_Baz.GetILGenerator ();
				il.Emit (SRE.OpCodes.Ldc_I4_0);
				il.Emit (SRE.OpCodes.Ret);

				property.SetSetMethod (set_Baz);
				property.SetGetMethod (get_Baz);

				type.CreateType ();
			});

			var bar = module.GetType ("Foo.Bar");
			var baz = bar.Properties.Single(m => m.Name == "Baz");

			Assert.AreEqual ("set_Baz", baz.SetMethod.Name);
			Assert.AreEqual ("get_Baz", baz.GetMethod.Name);
		}
	}
}
