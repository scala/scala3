# End Markers for Method Blocks

End markers for method blocks allow you to explicitly mark the end of method application blocks that use colon syntax (braceless arguments).

## Syntax

The syntax follows the pattern:

```scala
methodName(args):
  // block content
end methodName
```

## Examples

### Simple Method Blocks

```scala
import scala.language.experimental.endMarkersForMethodBlocks

def test(name: String)(body: => Unit): Unit =
  println(s"Running test: $name")
  body

test("my test"):
  val x = 1
  assert(x > 0)
end test
```

### Nested Calls

```scala
def foo(x: Int)(body: => Int): Int = body

test("my test"):
  foo(42):
    val result = 42 * 2
    result
  end foo
end test
```

### Apply Method Handling

When dealing with `apply` methods, the end marker follows the explicit method name used in the call:

**Explicit `apply` calls**: Use `end apply` when the method is called explicitly with `.apply`.

```scala
object Foo:
  def apply(block: => Unit): Unit = ()

Foo.apply:
  // do something
end apply
```

**Implicit `apply` calls**: Use the name of the object/class instance that owns the `apply` method when it's called implicitly.

```scala
object Foo:
  def apply(block: => Unit): Unit = ()

Foo:
  // do something
end Foo
```

```scala
class Foo:
  def apply(block: => Unit): Unit = ()

val foo = new Foo
foo:
  // do something
end foo
```

This rule ensures that the end marker always corresponds to the syntactically visible method name, making the code self-documenting and consistent with the principle that end markers should match the surface syntax.

## How to Enable

To use end markers for method blocks, you need to enable the experimental feature:

```scala
import scala.language.experimental.endMarkersForMethodBlocks
```

Alternatively, you can enable it globally with the compiler flag:

```
-language:experimental.endMarkersForMethodBlocks
```

## When to Use

End markers for method blocks are particularly useful when:

- You have deeply nested method calls with colon syntax
- You want to improve code readability by explicitly marking block boundaries
- You're working with DSLs or testing frameworks that use method blocks extensively

## Limitations

- End markers only work with method applications that use colon syntax (braceless arguments)
- The end marker name must exactly match the method name
- This feature is experimental and may undergo API changes in future releases

## Error Cases

The compiler will report errors for misaligned end markers:

```scala
test("my test"):
  val x = 1
  assert(x > 0)
end wrong  // Error: misaligned end marker
```

## Interaction with Other Features

This feature works alongside the existing `fewerBraces` feature and follows the same syntactic patterns. It extends the end marker functionality to method application blocks.
