object Foo:
  def unapply[T <: Tuple](tup: T): String *: String *: T =
    "a" *: "b" *: tup

// like {pos,neg}/i15991, but with an abstract tuple tail
class Test:
  val tup2: String *: String *: EmptyTuple = ("c", "d")

  def test3 =
    val Foo(x, y, z) = tup2 // error: Wrong number of argument patterns for Foo; expected: (String, String, String, String)
    x + y + z

  def test3a =
    val x1x = tup2 match
      case Foo(x, y, z) => // error: Wrong number of argument patterns for Foo; expected: (String, String, String, String)
        (x, y, z)
    val x = x1x._1
    val y = x1x._2
    val z = x1x._3
    x + y + z
