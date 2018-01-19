object O {
  sealed trait Nat
  type Zero = Nat
  sealed trait Succ[N <: Nat] extends Nat

  sealed trait NVec[N <: Nat, +A]
  case object NEmpty extends NVec[Zero, Nothing]
  case class NCons[N <: Nat, +A](head: A, tail: NVec[N, A]) extends NVec[Succ[N], A]

  def nzip[N <: Nat, A, B](v1: NVec[N, A], v2: NVec[N, B]): NVec[N, (A, B)] =
    (v1, v2) match {
      case (NEmpty, NEmpty) => NEmpty
      case (NCons(a, atail), NCons(b, btail)) =>
        NCons((a, b), nzip(atail, btail))
    }
}
