class Foo {
  val x: String = null // error: String is non-nullable

  def foo(x: String): String = "x"

  val y = foo(null) // error: String argument is non-nullable

  val z: String = foo("hello")

  class Bar
  val b: Bar = null // error: user-created classes are also non-nullable
}