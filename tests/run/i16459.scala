object Test {
  import scala.xml.*
  def main(args: Array[String]): Unit = {

  val xml = if(true) {
    <script type="text/javascript">
      'location.reload()'
      'foo bar'
     </script>
  } else <div>empty</div>

  assert(
    xml match
      case elm: Elem if
        elm.label == "script"
        && elm.child.length == 1
        && elm.child(0) == Atom(Text("\n      'location.reload()'\n      'foo bar'\n     "))
          => true
      case _ => false
      ,
      xml
    )
  // Scala 3 syntax
  val auxiliary0 = if true then {
    <script type="text/javascript">
      'location.reload()'
      'foo bar'
     </script>
  } else <div>empty</div>

  val auxiliary1 = if true then
    <script type="text/javascript">
      'location.reload()'
      'foo bar'
     </script>
  else <div>empty</div>
  
  val auxiliary2 = if true then <div>A</div>else <div>B</div>

  // Note:
  // This does not pass in Scala 2.12.18 and 2.13.12
  // due to "Sequence argument type annotation `: _*` cannot be used here:"
  val auxiliary3 = if(true) <div>A</div>else <div>B</div>
  
  // Note: This passes in Scala  2.12.18 and 2.13.12 too.
  val auxiliary4 = if(true) <div attr="...">A</div>else <div attr="...">B</div>

  // Pattern match without guard.
  // Note: This passes in Scala  2.12.18 and 2.13.12 too.
  val auxiliary5 = for (case _ @ <div>empty</div> <- Seq(xml)) yield ()
  // Note: These pass in Scala  2.12.18 and 2.13.12.
  val auxiliary6 = for (case _ @ <div>empty</div><- Seq(xml)) yield ()
  val auxiliary7 = for (case _ @ <div>empty</div><-Seq(xml)) yield ()
  // Pattern match with if guard.
  // Note: This passes in Scala  2.12.18 and 2.13.12 too.
  val auxiliary8 = for (case _ @ <foo>FooBar</foo> <- Seq(xml) if true)
  // Note: These pass in Scala  2.12.18 and 2.13.12.
  val auxiliary9 = for (case _ @ <foo>FooBar</foo><- Seq(xml) if true)
  val auxiliary10 = for (case _ @ <foo>FooBar</foo><-Seq(xml) if true)
   yield ()
  
  }

}

package scala.xml {
  type MetaData = AnyRef

  class UnprefixedAttribute(
    val key: String,
    val value: Text,
    next1: MetaData
  ) extends MetaData

  trait NamespaceBinding
  object TopScope extends NamespaceBinding
  object Null
  abstract class Node {
    def label: String
    def child: Seq[Node]
    override def toString = label + child.mkString
  }

  class Elem(prefix: String, val label: String, attributes1: MetaData, scope: NamespaceBinding, minimizeEmpty: Boolean, val child: Node*) extends Node
  object Elem {
    def unapply(e:Elem):Option[(String,String,Any,Text,Any)] = Some(("dummy","dummy",null,null,null))
  }
  class NodeBuffer extends Seq[Node] { 
    val nodes = scala.collection.mutable.ArrayBuffer.empty[Node]
    def &+(o: Any): NodeBuffer = o match {
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