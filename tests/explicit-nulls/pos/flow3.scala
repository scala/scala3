// TODO: broken
// Test that flow inference can look inside type ascriptions.
// This is useful in combination with inlining (because inlined methods have an ascribed return type).

class Foo {
  val x: String|Null = "hello"

  if (x != null) {
    val y = x.length
  }

  if ((x != null): Boolean) {
    val y = x.length
  }
}
