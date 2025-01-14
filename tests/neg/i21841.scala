object Test {

  sealed trait T
  sealed trait Arrow[A, B]

  type ArgsTo[S1, Target] <: NonEmptyTuple = S1 match {
    case Arrow[a, Target] => Tuple1[Expr[a]]
    case Arrow[a, b] => Expr[a] *: ArgsTo[b, Target]
  }

  sealed trait Expr[S] :
    def unapplySeq[Target](e: Expr[Target]): Option[ArgsTo[S, Target]] = ???

  case class Variable[S](id: String) extends Expr[S]

  val v = Variable[Arrow[T, Arrow[T, T]]]("v")
  val e : Expr[T] = ???

  e match
    case v[T](l, r) => ()  // error
    case _ => ()
}
