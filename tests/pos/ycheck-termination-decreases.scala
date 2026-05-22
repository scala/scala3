//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates, decreases}

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def f(logger: List[Nat], n: Nat): (Nat, List[Nat]) @decreases(n) =
    n match
      case Zero => (Zero, Zero :: logger)
      case Succ(m) => f(n :: logger, m)

}

