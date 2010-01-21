using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

using NUnit.Framework;

namespace Mono.Reflection {

	[TestFixture]
	public class DisassemblerTest {

		[Test]
		public void DefaultConstructor ()
		{
			AssertMethod (@"
	IL_0000: ldarg.0
	IL_0001: call void System.Object::.ctor()
	IL_0006: ret
", ".ctor");
		}

		[Test]
		public void ReadField ()
		{
			AssertMethod (@"
	IL_0000: ldarg.0
	IL_0001: ldfld string Test::str
	IL_0006: ret
", "ReadField");
		}

		[Test]
		public void WriteField ()
		{
			AssertMethod (@"
	IL_0000: ldarg.0
	IL_0001: ldarg val
	IL_0005: stfld string Test::str
	IL_000a: ret
", "WriteField");
		}

		[Test]
		public void TypeOfString ()
		{
			AssertMethod (@"
	IL_0000: ldtoken string
	IL_0005: call System.Type System.Type::GetTypeFromHandle(System.RuntimeTypeHandle)
	IL_000a: ret
", "TypeOfString");
		}

		[Test]
		public void ShortBranch ()
		{
			AssertMethod (@"
	IL_0000: ldarg.1
	IL_0001: ldc.i4.0
	IL_0002: ble.s IL_0006
	IL_0004: ldarg.1
	IL_0005: ret
	IL_0006: ldc.i4.m1
	IL_0007: ret
", "ShortBranch");
		}

		[Test]
		public void Branch ()
		{
			AssertMethod (@"
	IL_0000: ldarg.1
	IL_0001: ldc.i4.0
	IL_0002: ble IL_0009
	IL_0007: ldarg.1
	IL_0008: ret
	IL_0009: ldc.i4.m1
	IL_000a: ret
", "Branch");
		}

		[Test]
		public void Switch ()
		{
			AssertMethod (@"
	IL_0000: ldarg.1
	IL_0001: switch (IL_0018, IL_001a, IL_001c, IL_001e)
	IL_0016: br.s IL_001e
	IL_0018: ldc.i4.0
	IL_0019: ret
	IL_001a: ldc.i4.1
	IL_001b: ret
	IL_001c: ldc.i4.2
	IL_001d: ret
	IL_001e: ldc.i4.m1
	IL_001f: ret
", "Switch");
		}

		static void AssertMethod (string code, string method_name)
		{
			var method = GetMethod (method_name);
			Assert.AreEqual (Normalize (code), Normalize (Formatter.FormatMethodBody (method)));
		}

		static string Normalize (string str)
		{
			return str.Trim ().Replace ("\r\n", "\n");
		}

		static MethodBase GetMethod (string name)
		{
			return test_target.GetType ("Test").GetMember (name,
				BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance) [0] as MethodBase;
		}

		static Assembly test_target = LoadTestTarget ();

		static Assembly LoadTestTarget ()
		{
			var stream = Assembly.GetExecutingAssembly ().GetManifestResourceStream ("target.dll");
			return Assembly.Load (ToArray (stream));
		}

		static byte [] ToArray (Stream stream)
		{
			var buffer = new byte [16 * 1024];
			using (MemoryStream ms = new MemoryStream ()) {
				int read;
				while ((read = stream.Read (buffer, 0, buffer.Length)) > 0)
					ms.Write (buffer, 0, read);

				return ms.ToArray ();
			}
		}
	}
}
