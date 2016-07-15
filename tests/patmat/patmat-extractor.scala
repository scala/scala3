sealed trait Node
case class NodeA(i: Int) extends Node
case class NodeB(b: Boolean) extends Node
case class NodeC(s: String) extends Node

object Node {
  def unapply(node: Node): Option[(Node, Node)] = ???
}

// currently scalac can't do anything with following
// it's possible to do better in our case
object Test {
  def foo(x: Node): Boolean = x match { // unexhaustive
    case Node(NodeA(_), NodeB(_)) => true
    case Node(NodeA(4), NodeB(false)) => true // unreachable code
  }
}