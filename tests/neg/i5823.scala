// Test that `C|Null` is erased to `C` if `C` is
// a reference type.
// If `C` is a value type, then `C|Null = Object`.
// Ditto for `C|Nothing`.

class A
class B

class Foo {

  // ok, because A and B are <: Object.
  def foo(a: A|Null): Unit = ()
  def foo(b: B|Null): Unit = ()

  def bar(a: Int|Null): Unit = ()
  def bar(b: Boolean|Null): Unit = () // error: signatures match

  // ok, T is erased to `String` and `Integer`, respectively
  def gen[T <: String](s: T|Null): Unit = ()
  def gen[T <: Integer](i: T|Null): Unit = ()

  def gen2[T <: Int](i: T|Null): Unit = ()
  def gen2[T <: Boolean](b: T|Null): Unit = () // error: signatures match

  // ok, because A and B are <: Object.
  def foo2(a: A|Nothing): Unit = ()
  def foo2(b: B|Nothing): Unit = ()

  // ok because erased to primitive types
  def bar2(a: Int|Nothing): Unit = ()
  def bar2(b: Boolean|Nothing): Unit = ()

  // ok, T is erased to `String` and `Integer`, respectively
  def gen3[T <: String](s: T|Nothing): Unit = ()
  def gen3[T <: Integer](i: T|Nothing): Unit = ()

  // ok because erased to primitive types
  def gen4[T <: Int](i: T|Nothing): Unit = ()
  def gen4[T <: Boolean](b: T|Nothing): Unit = ()
}
