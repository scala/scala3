
object Test {
  import scala.xml.*
  def main(args: Array[String]): Unit = {
    val xml =  <div>FooBar</div><!-- /.modal-content -->
    assert(
      xml match
        case Seq(elm: Elem, comment: Comment) if
            elm.label == "div" &&
            elm.child(0) == Atom(Text("FooBar")) &&
            comment.label == " /.modal-content "
              => true
        case _ => false
      ,
      xml
    )
  }
}

package scala.xml {
  type MetaData = AnyRef

  trait NamespaceBinding
  object TopScope extends NamespaceBinding
  object Null
  abstract class Node {
    def label: String
    def child: Seq[Node]
    override def toString = label + child.mkString
  }
  class Comment(commentText: String) extends Node{
    def label = commentText
    def child = Nil
  }
  class Elem(prefix: String, val label: String, attributes1: MetaData, scope: NamespaceBinding, minimizeEmpty: Boolean, val child: Node*) extends Node
  class NodeBuffer extends Seq[Node] { 
    val nodes = scala.collection.mutable.ArrayBuffer.empty[Node]
    def &+(o: Any): NodeBuffer =
      o match {
        case n: Node => nodes.addOne(n) ; this
        case t: Text => nodes.addOne(Atom(t)) ; this
      }
    // Members declared in scala.collection.IterableOnce
    def iterator: Iterator[scala.xml.Node] = nodes.iterator
    // Members declared in scala.collection.SeqOps
    def apply(i: Int): scala.xml.Node = nodes(i)
    def length: Int = nodes.length
  }
  case class Text(text: String)
  case class Atom(t: Text) extends Node {
    def label = t.text
    def child = Nil
  }
}