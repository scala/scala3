//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  @terminates
  def loop(l: List[Int]): List[Int] =
    loop(0 :: l) // error

  @terminates
  def sum(l: List[Int]): Int =
    l match {
      case Nil => 0
      case x :: xs =>
        x + sum(xs) // error
    }

}

