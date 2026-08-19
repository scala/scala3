//> using options -Yexplicit-nulls

import language.experimental.magic
import scala.language.implicitConversions

object Test extends App {
  case class Node(x: Int)

  implicit def sc2xml(sc: StringContext): XMLContext = new XMLContext(sc)
  class XMLContext(sc: StringContext) {
    object xml {
      def unapplySeq(xml: Node): Seq[Node]? = List(Node(1))
    }
  }

  val x: Node = Node(0)
  x match { case xml"""<foo arg=$a/>""" => println(a) }
}
