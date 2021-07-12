// Test that flow typing works well with implicit resolution.

class Test {
  implicit val x: String | Null = ???
  summon[x.type <:< String] // error: x.type is widened String|Null

  if (x != null) {
    summon[x.type <:< String] // ok: x.type is widened to String
  }
}
