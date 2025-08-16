
object Test {
  import scala.xml._

  def main(args: Array[String]): Unit = {
    val xml =  <hello>world</hello>
    assert(xml.toString == "helloworld")
    val sq: NodeBuffer = <hello/><world/>
    assert(sq.mkString == "helloworld")

    val subSq: Node = <a><b/><c/></a>
    assert(subSq.child.toString == "Vector(b, c)") // implementation detail

    val attrSeq: Elem = <a foo="txt&entityref;txt"/>
    assert(attrSeq.attributes.asInstanceOf[UnprefixedAttribute].value.toString == "Vector(txt, &entityref;, txt)")

    val g: Group = <xml:group><a/><b/><c/></xml:group>
    assert(g.nodes.toString == "Vector(a, b, c)")
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
  class Elem(prefix: String, val label: String, val attributes: MetaData, scope: NamespaceBinding, minimizeEmpty: Boolean, val child: Node*) extends Node
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

  case class UnprefixedAttribute(key: String, value: Seq[Node], next: MetaData)
  case class EntityRef(entityName: String) extends Node {
    def label = s"&$entityName;"
    def child = Nil
  }

  case class Group(nodes: Seq[Node])
}
