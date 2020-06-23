// Test that `Null | Null | ... | Null` will not cause crash during typing.
// We want to strip `Null`s from the type after the `if` statement.

class Foo {
  def foo1: Unit = {
    val x: Null | Null | Null = ???
    if (x == null) return ()
    val y = x.length // error: x: Null is inferred
  }

  def foo2: Unit = {
    val x: Null | String | Null = ???
    if (x == null) return ()
    val y = x.length // ok: x: String is inferred
  }
}
