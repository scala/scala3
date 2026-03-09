//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class MyCons[+A](val head: A, val tail: MyList[A]) extends MyList[A]

  @terminates
  def sum(l: MyList[Int]): Int =
    l match {
      case MyNil => 0
      case MyCons(x, xs) => x + sum(xs)
    }

  @terminates
  def reverse[A](l: MyList[A], acc: MyList[A]): MyList[A] =
    l match {
      case MyNil => acc
      case MyCons(head, tail) => reverse(tail, MyCons(head, acc))
    }

  @terminates
  def merge(l1: MyList[Int], l2: MyList[Int]): MyList[Int] =
    l1 match {
      case MyNil => l2
      case MyCons(x, xs) =>
        l2 match {
          case MyNil => l1
          case MyCons(y, ys) =>
            if (x < y) then MyCons(x, merge(xs, l2))
            else MyCons(y, merge(l1, ys))
        }
    }

}

