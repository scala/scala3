//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def isEven(n: Nat): Boolean =
    n match
      case Zero => true
      case Succ(m) => isOdd(m)

  @terminates
  def isOdd(n: Nat): Boolean =
    n match
      case Zero => false
      case Succ(m) => isEven(m)


}

