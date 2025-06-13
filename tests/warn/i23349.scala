//> using options -Wunused:explicits,implicits

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
def copyFoo(foo: Foo)(a: String = foo.a, b: Int = foo.b): Foo = Foo(a, b)

class C:
  def copyFoo(foo: Foo, bar: String)(a: String = foo.a, b: Int = foo.b)(c: String = bar): Foo = Foo(a, b) // warn c
  def copyUsing(using foo: Foo, bar: String)(a: String = foo.a, b: Int = foo.b)(c: String = bar): Foo = // warn c
    Foo(a, b)

class K(k: Int)(s: String = "*"*k):
  override val toString = s

class KU(using k: Int)(s: String = "*"*k):
  override val toString = s

class KK(s: String):
  def this(k: Int)(s: String = "*"*k) = this(s)
  override val toString = s
