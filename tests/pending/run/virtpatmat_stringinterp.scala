
import scala.language.{ implicitConversions }

object Test extends dotty.runtime.LegacyApp {
  case class Node(x: Int)

  implicit def sc2xml(sc: StringContext): XMLContext = new XMLContext(sc)
  class XMLContext(sc: StringContext) {
    object xml {
      def unapplySeq(xml: Node): Option[Seq[Node]] = Some(List(Node(1)))
    }
  }

  val x: Node = Node(0)
  x match { case xml"""<foo arg=$a/>""" => println(a) }
}
