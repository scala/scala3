import scala.compiletime.ops.int.*

type Fill[N <: Int, A] <: Tuple = N match {
  case 0 => EmptyTuple
  case S[n] => A *: Fill[n, A]
}

sealed trait SeqToTuple[N <: Int] {
  def apply[A](s: Seq[A]): Fill[N, A]
}
implicit val emptyToZero: SeqToTuple[0] = new SeqToTuple[0] {
  override def apply[A](s: Seq[A]): EmptyTuple = EmptyTuple
}
implicit def successorToSuccessor[N <: Int](implicit pred: SeqToTuple[N]): SeqToTuple[S[N]] = new SeqToTuple[S[N]] {
  override def apply[A](s: Seq[A]): Fill[S[N], A] =
    s.head *: pred(s.tail)
}
