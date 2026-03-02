// Test that is fixed when explicit nulls are enabled.
// https://github.com/scala/scala3/issues/6247

class Foo {
  val x1: String|Null = null
  x1.nn.length
  val x2: String = x1.nn
  x1.nn.length
}
