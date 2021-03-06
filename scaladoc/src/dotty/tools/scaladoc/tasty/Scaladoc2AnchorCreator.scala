package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.util.matching.Regex

trait Scaladoc2AnchorCreator:
  self: SymOps[_] =>

  import self.q.reflect._

  implicit private val printer: Printer[Tree] = Printer.TreeShortCode

  def getScaladoc2Type(t: Tree) = {
    (t match {
    case d: DefDef => d.show.split("def", 2)(1)
    case t: TypeDef => t.show.split("type", 2)(1)
    case v: ValDef => v.show.split("val|var", 2)(1)
  }).replace(" ","")
}