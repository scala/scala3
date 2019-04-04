// Test that nullable types can be represented via unions.

class Bar

class Foo {
  val x: String|Null = null 
  val y: Array[String]|Null = null
  val b: Null|Bar = null

  def foo(p: Bar|Null): String|Null = null

  foo(null)
  foo(b)
}
