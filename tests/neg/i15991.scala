object Foo:
  def unapply(x: Any): String *: String *: EmptyTuple = ("a", "b")

class Test:
  def test =
    val Foo(x, y, z) = 1 // error: Wrong number of argument patterns for Foo; expected: (String, String)
    x + y + z
