import Nat._

@main def Test: Unit =
  assert(toIntMacro(Succ(Succ(Succ(Zero)))) == 3)
  assert(toNatMacro(3) == Succ(Succ(Succ(Zero))))
  val zero: Zero.type = ZeroMacro
  assert(zero == Zero)
  assert(toIntMacro(toNatMacro(3)) == 3)
  val n: Succ[Succ[Succ[Zero.type]]] = toNatMacro(3)
  assert(toIntInline(n) == 3) // TODO: there is an error with positions when passing the result of toNatMacro(3) directly
