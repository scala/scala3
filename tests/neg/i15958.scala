sealed trait NatT { type This <: NatT }
case class Zero() extends NatT {
  type This = Zero
}
case class Succ[N <: NatT](n: N) extends NatT {
  type This = Succ[n.This]
}

trait IsLessThan[+M <: NatT, N <: NatT]
object IsLessThan:
  given base[M <: NatT]: IsLessThan[M, Succ[M]]()
  given weakening[N <: NatT, M <: NatT] (using IsLessThan[N, M]): IsLessThan[N, Succ[M]]()
  given reduction[N <: NatT, M <: NatT] (using IsLessThan[Succ[N], Succ[M]]): IsLessThan[N, M]()

sealed trait UniformTuple[Length <: NatT, T]:
  def apply[M <: NatT](m: M)(using IsLessThan[m.This, Length]): T

case class Empty[T]() extends UniformTuple[Zero, T]:
  def apply[M <: NatT](m: M)(using IsLessThan[m.This, Zero]): T = throw new AssertionError("Uncallable")

case class Cons[N <: NatT, T](head: T, tail: UniformTuple[N, T]) extends UniformTuple[Succ[N], T]:
  def apply[M <: NatT](m: M)(using proof: IsLessThan[m.This, Succ[N]]): T = m match
    case Zero() => head
    case m1: Succ[predM] =>
      val proof1: IsLessThan[m1.This, Succ[N]] = proof

      val res0 = tail(m1.n)(using IsLessThan.reduction(using proof))  // error // limitation
      val res1 = tail(m1.n)(using IsLessThan.reduction(using proof1))
      res1
