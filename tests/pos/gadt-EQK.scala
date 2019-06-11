object EQK {
  sealed trait EQ[A, B]
  final case class Refl[A]() extends EQ[A, A]

  sealed trait EQK[F[_], G[_]]
  final case class ReflK[F[_]]() extends EQK[F, F]

  def m0[F[_], G[_], A](fa: F[A], eqk: EQK[F, G]): G[A] =
    eqk match { case ReflK() => fa }

  def m1[F[_], G[_], A](fa: F[A], eq: EQ[A, Int], eqk: EQK[F, G]): G[Int] =
    eqk match {
      case ReflK() => eq match {
        case Refl() =>
          val r1: F[Int] = fa
          val r2: G[A] = fa
          val r3: F[Int] = r2
          fa : G[Int]
      }
    }
}
