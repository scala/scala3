// Test that array contents are non-nullable.
class Foo {
  val x: Array[String] = Array("hello")
  val s: String = x(0)
}
