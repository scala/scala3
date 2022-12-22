object Foo:
  def unapply(x: Any): String *: String *: EmptyTuple =
    ("a", "b")

class Test:
  def test =
    val Foo(x, y) = 1
    x + y
