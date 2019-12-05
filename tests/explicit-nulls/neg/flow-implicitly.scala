
// Test that flow typing works well with implicit resolution.
class Test {
  implicit val x: String | Null = ???
  implicitly[x.type <:< String] // error: x.type is widened String|Null

  if (x != null) {
    implicitly[x.type <:< String] // ok: x.type is widened to String
  }
}
