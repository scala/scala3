package dotty.tools.dotc
package transform.localopt

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.ast.desugar

class DropForMap extends MiniPhase:
  import DropForMap.*

  override def phaseName: String = DropForMap.name

  override def description: String = DropForMap.description

  override def transformApply(tree: Apply)(using Context): Tree =
    if !tree.hasAttachment(desugar.TrailingForMap) then tree
    else tree match
      case aply @ Apply(MapCall(f), List(Lambda(List(param), body)))
      if f.tpe =:= aply.tpe => // make sure that the type of the expression won't change
        f // drop the map call
      case _ =>
        tree.removeAttachment(desugar.TrailingForMap)
        tree

  private object Lambda:
    def unapply(tree: Tree)(using Context): Option[(List[ValDef], Tree)] =
      tree match
        case Block(List(defdef: DefDef), Closure(Nil, ref, _))
        if ref.symbol == defdef.symbol && !defdef.paramss.exists(_.forall(_.isType)) =>
          Some((defdef.termParamss.flatten, defdef.rhs))
        case _ => None

  private object MapCall:
    def unapply(tree: Tree)(using Context): Option[Tree] = tree match
      case Select(f, nme.map) => Some(f)
      case Apply(fn, _) => unapply(fn)
      case TypeApply(fn, _) => unapply(fn)
      case _ => None

object DropForMap:
  val name: String = "dropForMap"
  val description: String = "Drop unused trailing map calls in for comprehensions"
