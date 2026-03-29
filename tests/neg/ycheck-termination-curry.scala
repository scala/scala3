//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates, decreasesBy}

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  @terminates
  def f(a: Nat)(b: Nat): Nat =
    a match {
      case Zero => b
      case Succ(n) =>
        val g = f(a) // error
        g(b)
    }

}


