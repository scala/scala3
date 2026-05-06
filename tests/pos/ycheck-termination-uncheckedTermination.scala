//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates
  import scala.util.uncheckedTermination

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def f(n: Nat): Boolean =
    n match
      case Zero => false
      case Succ(m) =>
        // The below does not terminate but
        // is assumed terminating by the user.
        uncheckedTermination:
          f(n) || f(Succ(n))

}

