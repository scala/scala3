package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.util.matching.Regex

object Scaladoc2AnchorCreator:

  def getScaladoc2Type(using Quotes)(t: reflect.Tree) =
    import reflect.*
    val regex = t match
      case d: DefDef =>  "def"
      case t: TypeDef => "type"
      case v: ValDef =>  "val|var"
    t.show(using Printer.TreeShortCode).split(regex, 2)(1).replace(" ","")
