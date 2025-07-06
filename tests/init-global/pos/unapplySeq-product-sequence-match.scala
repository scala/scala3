trait Node {
  val prefix = 5
  val child = Array(3)
}

class SpecialNode extends Node

class Group extends Node

class C extends Node

object Elem {
  def apply(prefix: Int, children: Int*) = new C
  def unapplySeq(n: Node) = n match {
    case _: SpecialNode | _: Group => None
    case _                         => Some((n.prefix, n.child.toSeq))
  }
}

object O {
  def updateNode(node: Node): Node =
    node match {
      case Elem(prefix, children @ _*) =>
        Elem(prefix, children*)
      case other => other
    }

  val a = updateNode(new Group)
}
