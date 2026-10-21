trait Nat
case object Zero extends Nat
case class Succ[N <: Nat](pred: N) extends Nat

transparent inline def s(inline y: Nat): Nat = Succ(y)

val d: Succ[Zero.type] = s(Zero) // error
