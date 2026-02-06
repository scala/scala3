object Unpack {
  trait Expr[T] {
    type Fields = NamedTuple.From[T]
  }
  final case class Pair(a: Int, b: Int)
  def unapply(e: Expr[Pair]): e.Fields = ???

  val x: Expr[Pair] = ???
  x match {
    case Unpack(_, _) => ???
  }
}
