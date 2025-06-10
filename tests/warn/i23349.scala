//> using options -Wunused:explicits

// An external class that doesn't get its own `copy` method.
class Foo(val a: String, val b: Int)

//
// Example 1: add `copy` method via an extension method.
//
extension (self: Foo)
  def copy(a: String = self.a, b: Int = self.b): Foo = Foo(a, b) // nowarn

//
// Example 2: implement `copyFoo` with parameter groups.
//
def copyFoo(foo: Foo)(a: String = foo.a, b: Int = foo.b): Foo = Foo(a, b) // warn
