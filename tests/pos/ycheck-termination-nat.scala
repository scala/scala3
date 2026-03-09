//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  sealed abstract class Nat {

    @terminates
    def +(that: Nat): Nat =
      this match {
        case Zero => that
        case Succ(n) => Succ(n + that)
      }

  }
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def ackermann(n: Nat, m: Nat): Nat = {
    val a = n
    a match {
      case Zero => Succ(m)
      case Succ(nm1) =>
        m match {
          case Zero => ackermann(nm1, Succ(Zero))
          case Succ(mm1) => ackermann(nm1, ackermann(n, mm1))
        }
    }
  }

}

