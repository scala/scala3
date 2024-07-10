package dotty.tools.dotc
package transform.localopt

import scala.language.unsafeNulls

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.ConstFold
import dotty.tools.dotc.ast.desugar
import scala.util.chaining.*

class DropForMap extends MiniPhase:
  import tpd.*

  override def phaseName: String = DropForMap.name

  override def description: String = DropForMap.description

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    if !tree.hasAttachment(desugar.TrailingForMap) then tree.tap(_.removeAttachment(desugar.TrailingForMap))
    else tree match
      case Apply(MapCall(f), List(Lambda(List(param), body)))
      if isEssentiallyUnitLiteral(param, body) && param.tpt.tpe.isRef(defn.UnitClass) =>
        f
      case _ =>
        tree.tap(_.removeAttachment(desugar.TrailingForMap))

  private object Lambda:
    def unapply(tree: Tree)(using Context): Option[(List[ValDef], Tree)] =
      tree match
        case Block(List(defdef: DefDef), Closure(Nil, ref, _)) if ref.symbol == defdef.symbol && !defdef.paramss.exists(_.forall(_.isType)) =>
          Some((defdef.termParamss.flatten, defdef.rhs))
        case _ => None

  private object MapCall:
    def unapply(tree: Tree)(using Context): Option[Tree] = tree match
      case Select(f, nme.map) => Some(f)
      case Apply(fn, _) => unapply(fn)
      case TypeApply(fn, _) => unapply(fn)
      case _ => None

  def isEssentiallyUnitLiteral(param: ValDef, tree: Tree)(using Context): Boolean = tree match
    case Literal(Constant(())) => true
    case Match(scrutinee, List(CaseDef(_, EmptyTree, body))) => isEssentiallyUnitLiteral(param, body)
    case Block(Nil, expr) => isEssentiallyUnitLiteral(param, expr)
    case _ => false

object DropForMap:
  val name: String = "dropForMap"
  val description: String = "Drop unused trailing map calls in for comprehensions"
