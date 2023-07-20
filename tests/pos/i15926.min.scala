// like pos/i15926.scala
// but minimised to the subset of paths needed
// to fail the specific test case
sealed trait Nat
final case class Zero() extends Nat
final case class Succ[+N <: Nat]() extends Nat

final case class Neg[+N <: Succ[Nat]]()

type Sum[X, Y] = Y match
  case Zero    => X
  case Succ[y] => Sum[Succ[X], y]

type IntSum[A, B] = B match
  case Neg[b] => A match
    case Neg[a] => Neg[Sum[a, b]]

type One = Succ[Zero]
type Two = Succ[One]

class Test:
  def test() = summon[IntSum[Neg[One], Neg[One]] =:= Neg[Two]]
