object IdrisVect {
  sealed trait Vect[T]
  case class Cons[Q](head: Q, tail: Vect[Q]) extends Vect[Q]

  def length[S](xs: Vect[S]): Unit =
    length(xs.asInstanceOf[Cons[_]].tail)
}
