//> using options -Werror
sealed trait Nat
case class Zero() extends Nat
case class Succ[N <: Nat](n: N) extends Nat

class Test:
  def foo(n: Nat): Int = n match
    case Zero() => 0
    case Succ(Zero()) => 1
    case _ => 2 // was: warning for this line

  def test = foo(Succ(Succ(Zero()))) // evaluates to 2
