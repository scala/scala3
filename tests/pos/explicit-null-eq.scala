// Test that non-nullable types can be still be compared
// for equality against null.
class Foo {
  val x: String = "hello"
  if (x != null) { // allowed as escape hatch
  }
  if (x == null) {
  }
}
