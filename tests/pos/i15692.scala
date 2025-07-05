sealed trait Nat
sealed trait Succ[Prev <: Nat] extends Nat
sealed trait Zero extends Nat

class Sum[M <: Nat, N <: Nat] {
  type Out <: Nat
}

object Sum {
  type Aux[M <: Nat, N <: Nat, R <: Nat] = Sum[M, N] { type Out = R }

  implicit def sum0[N <: Nat]: Sum.Aux[Zero, N, N] = new Sum[Zero, N] { type Out = N }
  implicit def sum1[M <: Nat, N <: Nat, R <: Nat](implicit sum: Sum.Aux[M, Succ[N], R]): Sum.Aux[Succ[M], N, R] =
    new Sum[Succ[M], N] { type Out = R }
}

object Test {
  def main(args: Array[String]): Unit = {
    type _3 = Succ[Succ[Succ[Zero]]]
    type _5 = Succ[Succ[_3]]

    implicitly[Sum[_3, _5]]
  }
}
