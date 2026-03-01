//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  class ClassNode(val next: ClassNode)

  @terminates
  def classLoop(r: ClassNode): Int = // error
    1 + classLoop(r.next)

  @terminates
  def sumStd(l: List[Int]): Int = // error
    l match {
      case Nil => 0
      case x :: xs => x + sumStd(xs)
    }

}

