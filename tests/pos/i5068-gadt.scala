object i5068 {
  case class Box[F[_]](value: F[Int])
  sealed trait IsK[F[_], G[_]]
  final case class ReflK[F[_]]() extends IsK[F, F]

  def foo[F[_], G[_]](r: F IsK G, a: Box[F]): Box[G] = r match { case ReflK() => a }
}

object i5068b {
  type WeirdShape[A[_], B] = A[B]
  // type WeirderShape[S[_[_], _], I, M] = Any
  case class Box[ F[_[_[_], _], _, _[_]] ](value: F[WeirdShape, Int, Option])
  sealed trait IsK[F[_[_[_], _], _, _[_]], G[_[_[_], _], _, _[_]]]
  final case class ReflK[ F[_[_[_], _], _, _[_]] ]() extends IsK[F, F]

  def foo[F[_[_[_], _], _, _[_]], G[_[_[_], _], _, _[_]]](
    r: F IsK G,
    a: Box[F]
  ): Box[G] = r match { case ReflK() => a }

  // def main(args: Array[String]): Unit = {
  //   println(foo(ReflK(), Box[WeirderShape](???)))
  // }
}
