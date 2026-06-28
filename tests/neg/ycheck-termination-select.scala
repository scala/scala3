//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class MyCons[+A](val head: A, val tail: MyList[A]) extends MyList[A] {
    def call: Nat =
      head match
        case a: Nat => loop(a)
        case _ => Zero
  }

  def loop(a: Nat): Nat = loop(Succ(a)) // error

  @terminates
  def f(a: Nat): Nat =
    MyCons(a, MyNil).call
}

