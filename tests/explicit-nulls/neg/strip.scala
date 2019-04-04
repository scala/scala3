
class Foo {

  class B1
  class B2

  val x: (Null | String) | Null |  (B1 | (Null | B2)) = ???
  if (x != null) {
    val x2: String | B1 | B2 = x // ok: can remove all nullable unions
  }

  val x2: (Null | String) & (Null | B1) = ???
  if (x2 != null) {
    val x3: String & B1 = x2 // error: can't remove null from embedded intersection
  }
}
