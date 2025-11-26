//> using options -Werror

object Unpack {
  final case class Pair(a: Int, b: Int)
  def unapply(e: Pair): NamedTuple.NamedTuple[("a", "b"), (Int, Int)] = ???

  val x: Pair = ???
  x match {
    case Unpack(_, _) => ???
  }
}
