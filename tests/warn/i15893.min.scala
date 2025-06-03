sealed trait NatT
case class Zero()                 extends NatT
case class Succ[+N <: NatT](n: N) extends NatT

type Mod2[N <: NatT] <: NatT = N match
  case Zero                  => Zero
  case Succ[Zero]            => Succ[Zero]
  case Succ[Succ[predPredN]] => Mod2[predPredN]

def dependentlyTypedMod2[N <: NatT](n: N): Mod2[N] = n match
  case Zero(): Zero                         => Zero() // warn
  case Succ(Zero()): Succ[Zero]             => Succ(Zero()) // warn
  case Succ(Succ(predPredN)): Succ[Succ[?]] => dependentlyTypedMod2(predPredN) // warn
