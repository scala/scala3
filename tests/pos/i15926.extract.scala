// like pos/i15926.scala
// but with the nested match type extracted
// which is a workaround that fixed the problem
sealed trait Nat
final case class Zero() extends Nat
final case class Succ[+N <: Nat]() extends Nat

final case class Neg[+N <: Succ[Nat]]()

type Sum[X, Y] = Y match
  case Zero    => X
  case Succ[y] => Sum[Succ[X], y]

type IntSum[A, B] = B match
  case Neg[b] => IntSumNeg[A, b]

type IntSumNeg[A, B] = A match
  case Neg[a] => Neg[Sum[a, B]]

type One = Succ[Zero]
type Two = Succ[One]

class Test:
  def test() = summon[IntSum[Neg[One], Neg[One]] =:= Neg[Two]]
