// Test that flow typing works well with singleton types.

class Test {
  val x : String | Null = ???
  if (x != null) {
    val y: x.type = x
    y.toLowerCase // ok: y should have type `String` in this branch
  }
}
