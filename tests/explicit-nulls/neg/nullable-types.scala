// Test that reference types are no longer nullable.

class Foo {
  val s: String = null          // error
  val s1: String | Null = null  // ok
  val b: Boolean = null         // error
  val ar: AnyRef = null         // error
  val a: Any  = null            // ok
  val n: Null = null            // ok

  def foo(x: String): String = "x"

  val y = foo(null) // error: String argument is non-nullable

  val z: String = foo("hello")

  class Bar
  val bar: Bar = null // error: user-created classes are also non-nullable
}
