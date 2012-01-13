## Mono.Reflection

Mono.Reflection is an helper library to complement the System.Reflection and System.Reflection.Emit namespaces.

It works on both Mono >= 2.8 and .net >= 4.0.

## API

***

```csharp
public sealed class Image {
	public static bool IsAssembly (string fileName) {}
	public static bool IsAssembly (Stream stream) {}
}
```

> Test whether a file is a managed assembly or not.

***

```csharp
public static class BackingFieldResolver {
	public static FieldInfo GetBackingField (this PropertyInfo self) {}
}
```

> Returns the field backing a property or throws an InvalidOperationException.

***

```csharp
public static class Disassembler {
	public static IList<Instruction> GetInstructions (this MethodBase self) {}
}
```

> Returns a read only collection of Instruction representing the
> IL method body or throws an ArgumentException if the method doesn't provide a body.

***

```csharp
public class Instruction {
	public int Offset { get; }
	public OpCode OpCode { get; }
	public object Operand { get; }

	public Instruction Next { get; }
	public Instruction Previous { get; }
}
```

> Represents an IL instruction.
