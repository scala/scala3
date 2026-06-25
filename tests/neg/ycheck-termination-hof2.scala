//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates

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
  case class Const(n: Int) extends Expr
  case class Add(args: MyList[Expr]) extends Expr

  @terminates
  def sum(l: MyList[Int]): Int =
    l.foldRight(0)(_ + _)

  @terminates
  def addAllWrong(e: Expr): Int =
    e match
      case Const(c) => c
      case Add(args) => args.foldRight(0)((left, right) => addAllWrong(e) + right) // error

}

