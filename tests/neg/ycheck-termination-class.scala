//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  class ClassNode(val next: ClassNode)

  @terminates
  def classLoop(r: ClassNode): Int =
    1 + classLoop(r.next) // error

}

