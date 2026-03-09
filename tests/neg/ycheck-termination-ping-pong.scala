//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def isEvenWrong(n: Nat): Boolean =
    def isOddWrong(n: Nat): Boolean =
      n match
        case Zero => false
        case Succ(_) => isEvenWrong(n) // error
    n match
      case Zero => true
      case Succ(_) => isOddWrong(n)

}


