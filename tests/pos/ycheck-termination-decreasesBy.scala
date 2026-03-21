//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates, decreasesBy}

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def f(logger: List[Nat], n: Nat): (Nat, List[Nat]) @decreasesBy(n) =
    n match
      case Zero => (Zero, logger :+ Zero)
      case Succ(m) => f(logger :+ n, m)

}

