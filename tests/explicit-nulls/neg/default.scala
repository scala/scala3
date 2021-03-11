class Foo {
  val s: String = null // error: String is non-nullable

  def foo(x: String): String = "x"

  val fn = foo(null) // error: String argument is non-nullable

  val x: String = foo("hello")

  val arr: Array[Int] = null // error: Array is non-nullable

  val t: (String, Int) = null // error: Tuple is non-nullable

  val f: String => Int = null // error: Function is non-nullable

  class Bar
  val b: Bar = null // error: user-created classes are also non-nullable
}