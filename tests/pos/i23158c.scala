//> using options -Werror

object Unpack {
  final case class Pair(a: Int, b: Int)
  def unapply(e: Pair): Some[(a: Int, b: Int)] = ???

  val x: Pair = ???
  x match {
    case Unpack(_, _) => ???
  }
}
