// Test that Null | Null will not cause crash during typing.

class Foo {
  def foo1: Unit = {
    val x: Null | Null | Null = ???
    if (x == null) return ()
    val y = x.length // error: x: Null is inferred
  }
}
