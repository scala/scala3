//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  class ClassNode(val next: ClassNode)

  @terminates
  def classLoop(r: ClassNode): Int =
    1 + classLoop(r.next) // error

  @terminates
  def sumStd(l: List[Int]): Int =
    l match {
      case Nil => 0
      case x :: xs => x + sumStd(xs) // error
    }

}

