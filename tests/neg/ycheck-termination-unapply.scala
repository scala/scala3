//> using options -Ycheck-termination

class C {
  import scala.annotation.terminates

  case class Node(value: Int, next: Node)
  object Node {
    def unapply(c: Node): Option[(Int, Node)] =
      Some((c.value, c))
  }

  @terminates
  def caseLoop(n: Node): Int = // error
    n match {
      case Node(v, next) =>
        v + caseLoop(next)
    }

}

