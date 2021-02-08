package dotty.tools
package dotc
package transform

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.Inliner

/** Check that `tree.rhs` can be right hand-side of an `inline` value definition. */
class InlineVals extends MiniPhase:
  import ast.tpd._

  def phaseName: String = "inlineVals"

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    if !ctx.erasedTypes then
      tree match
        case tree: ValDef => checkInlineConformant(tree)
        case _ =>

  override def transformValDef(tree: ValDef)(using Context): Tree =
    checkInlineConformant(tree)
    tree

  /** Check that `tree.rhs` can be right hand-side of an `inline` value definition. */
  private def checkInlineConformant(tree: ValDef)(using Context): Unit = {
    if tree.symbol.is(Inline, butNot = DeferredOrTermParamOrAccessor)
      && !Inliner.inInlineMethod
    then
      val rhs = tree.rhs
      val tpt = tree.tpt
      tpt.tpe.widenTermRefExpr.dealias.normalized match
        case tp: ConstantType =>
          if !isPureExpr(rhs) then
            val details = if enclosingInlineds.isEmpty then "" else em"but was: $rhs"
            report.error(s"inline value must be pure$details", rhs.srcPos)
        case _ =>
          val pos = if tpt.span.isZeroExtent then rhs.srcPos else tpt.srcPos
          report.error(em"inline value must have a literal constant type", pos)
  }