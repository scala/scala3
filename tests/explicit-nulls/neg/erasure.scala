// Check that T|Null is erased to T if T is a reference type.

trait Foo {
  def foo(s: String|Null): Unit
  def foo(s: String): Unit // error: collision after erasure
}
