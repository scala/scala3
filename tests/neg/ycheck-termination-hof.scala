//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates, decreasesBy}

  sealed abstract class Nat {
    def +(that: Nat): Nat =
      this match {
        case Zero => that
        case Succ(n) => n + Succ(that)
      }
  }
  case object Zero extends Nat
  case class Succ(val n: Nat) extends Nat

  sealed trait MyList[+A] {
    def foldRight[B](z: B)(op: (A, B) => B): B =
      this match {
        case MyNil => z
        case MyCons(head, tail) => op(head, tail.foldRight(z)(op))
      }
  }
  case object MyNil extends MyList[Nothing]
  case class MyCons[+A](val head: A, val tail: MyList[A]) extends MyList[A]

  sealed trait Expr
  case class Const(c: Nat) extends Expr
  case class Add(args: MyList[Expr]) extends Expr

  @terminates
  def addAllWrong(e: Expr): Nat = {
    e match
      case Const(c) => c
      case Add(args) => args.foldRight[Nat](Zero)((left, right) => addAllWrong(e) + right) // error
  }

  @terminates
  def addAllWrong2(e: Expr): Nat = {
    e match
      case Const(c) => c
      case Add(args) =>
        val op = (left: Expr, right: Nat) => addAllWrong2(e) + right // error
        args.foldRight[Nat](Zero)(op)
  }


}

