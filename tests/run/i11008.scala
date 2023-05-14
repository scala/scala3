object A:
  def unapply(s: String): String *: EmptyTuple = Tuple1(s)

object B:
  def unapply(s:String): String *: Int *: EmptyTuple = Tuple2(s, 10)

@main def Test =
  "hello" match
    case A(x) =>
      println(x)
  "hello" match
    case B(x, y) =>
      println(s"$x and $y")
