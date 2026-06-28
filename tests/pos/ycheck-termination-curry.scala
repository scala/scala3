//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates, decreases}

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def ackermann(m: Nat)(n: Nat): Nat @decreases(n) = {
    n match {
      case Zero => Succ(m)
      case Succ(pn) =>
        m match {
          case Zero => ackermann(Succ(Zero))(pn)
          case Succ(pm) =>
            val partial = ackermann(ackermann(pm)(n))
            partial(pn)
        }
    }
  }

}

