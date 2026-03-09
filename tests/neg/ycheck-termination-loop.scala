//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class MyCons[+A](val head: A, val tail: MyList[A]) extends MyList[A]

  @terminates
  def loop(l: MyList[Int]): Int =
    loop(MyCons(0, l)) // error

}

