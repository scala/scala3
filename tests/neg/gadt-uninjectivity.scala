object uninjectivity {
  sealed trait EQ[A, B]
  final case class Refl[T]() extends EQ[T, T]

  def absurd1[F[_], X, Y](eq: EQ[F[X], F[Y]], x: X): Y = eq match {
    case Refl() =>
      x // error
  }

  def absurd2[F[_], G[_]](eq: EQ[F[Int], G[Int]], fs: F[String]) = eq match {
    case Refl() =>
      val gs: G[String] = fs // error
      ()
  }

  def absurd3[F[_], G[_], X, Y](eq: EQ[F[X], G[Y]], fx: F[X]) = eq match {
    case Refl() =>
      val gx: G[X] = fx // error
      val fy: F[Y] = fx // error
      ()
  }
}
