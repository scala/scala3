object Foo:
  def unapply[T <: Tuple](tup: T): String *: String *: T =
    "a" *: "b" *: tup

// like {pos,neg}/i15991, but with an abstract tuple tail
class Test:
  val tup2: String *: String *: EmptyTuple = ("c", "d")

  def test2 =
    val Foo(x, y, _, _) = tup2
    x + y

  // like test2, but as the desugaring of what PatternDef's become
  def test2b =
    val x1x = tup2 match
      case Foo(x, y, _, _) =>
        (x, y)
    val x = x1x._1
    val y = x1x._2
    x + y
