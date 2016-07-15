sealed trait Node

class NodeType1 (val a:Int) extends Node
class NodeType2 (val b:Int) extends Node

object NodeType1 {
  def unapply (x : NodeType1) : Some[Int] = Some(x.a)
}

object NodeType2 {
  def unapply (x : NodeType2) : Some[Int] = Some(x.b)
}

object Test {
  def test (x: Node) = x match {
    case NodeType1(a) => "got node type 1 " + a
  }
}