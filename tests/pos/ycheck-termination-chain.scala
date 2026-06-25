//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def first(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(_) => f1(n)

  @terminates
  def f1(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(_) => f2(n)

  @terminates
  def f2(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(_) => decreases(n)

  @terminates
  def decreases(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(m) => f3(m)

  @terminates
  def f3(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(_) => f4(n)

  @terminates
  def f4(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(_) => last(n)

  @terminates
  def last(n: Nat): Nat =
    n match
      case Zero => Succ(Zero)
      case Succ(_) => first(n)


}

