// Test that `toString` has been special-cased to
// return a non-nullable value.

class Foo {
  val x: java.lang.Integer = 42
  val y: String = x.toString // would fail if toString returns nullable value
  val y2 = x.toString // test interaction with type inference
  val z: String = y2
}
