//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  @terminates
  def addOne(x: Option[Int]): Int =
    x match {
      case None => 0
      case Some(v) => 1 + addOne(x) // error
    }

  sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class MyCons[+A](val head: A, val tail: MyList[A]) extends MyList[A]

  @terminates
  def loop(l: MyList[Int]): Int =
    loop(MyCons(0, l)) // error

}

