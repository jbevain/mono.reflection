.assembly extern mscorlib
{
	.publickeytoken = (B7 7A 5C 56 19 34 E0 89)
	.ver 2:0:0:0
}

.assembly target {}

.module target.dll

.class private Test extends [mscorlib]System.Object {

	.method public specialname rtspecialname instance void .ctor () cil managed
	{
		ldarg.0
		call		instance void [mscorlib]System.Object::.ctor ()
		ret
	}

	.field private string str

	.method public instance string ReadField () cil managed
	{
		ldarg.0
		ldfld		string Test::str
		ret
	}

	.method public instance void WriteField (string val) cil managed
	{
		ldarg.0
		ldarg		val
		stfld		string Test::str
		ret
	}

	.method public instance class [mscorlib]System.Type TypeOfString () cil managed
	{
		ldtoken    [mscorlib]System.String
		call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle (valuetype [mscorlib]System.RuntimeTypeHandle)
		ret
	}

	.method public instance int32 ShortBranch (int32 i) cil managed
	{
		ldarg.1
		ldc.i4.0
		ble.s		negative

		ldarg.1
		ret

	negative:
		ldc.i4.m1
		ret
	}

	.method public instance int32 Branch (int32 i) cil managed
	{
		ldarg.1
		ldc.i4.0
		ble			negative

		ldarg.1
		ret

	negative:
		ldc.i4.m1
		ret
	}

	.method public instance int32 Switch (int32 i) cil managed
	{
		ldarg.1
		switch (zero, one, two, other) 
		br.s		other

	zero:
		ldc.i4.0
		ret

	one:
		ldc.i4.1
		ret

	two:
		ldc.i4.2
		ret

	other:
		ldc.i4.m1
		ret
	}
}
