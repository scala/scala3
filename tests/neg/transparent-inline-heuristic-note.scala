trait Nat
case object Zero extends Nat
case class Succ[N <: Nat](pred: N) extends Nat

transparent inline def s(inline y: Nat): Nat = Succ(y)

val x: String = s(Zero) // error
// limitation: note about transparent inline expansion is emitted
// even when the root cause of type mistmatch isn't transparent inline limitatation
