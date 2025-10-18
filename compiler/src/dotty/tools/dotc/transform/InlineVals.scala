package dotty.tools
package dotc
package transform

import core.*
import Contexts.*, Decorators.*, Symbols.*, Flags.*, Types.*
import MegaPhase.MiniPhase
import inlines.Inlines
import ast.tpd


/** Check that `tree.rhs` can be right hand-side of an `inline` value definition. */
class InlineVals extends MiniPhase:
  import ast.tpd.*

  override def phaseName: String = InlineVals.name

  override def description: String = InlineVals.description

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
      && !Inlines.inInlineMethod
    then
      val rhs = tree.rhs
      val tpt = tree.tpt
      tpt.tpe.widenTermRefExpr.dealiasKeepOpaques.normalized match
        case tp: ConstantType =>
          if !isPureExpr(rhs) then
            def details =
              if enclosingInlineds.nonEmpty || rhs.isInstanceOf[tpd.Inlined]
              then i" but was: $rhs"
              else ""
            report.error(em"inline value must be pure$details", rhs.srcPos)
        case tp =>
          if tp.typeSymbol.is(Opaque) then
            report.error(em"The type of an `inline val` cannot be an opaque type.\n\nTo inline, consider using `inline def` instead", rhs)
          else if tp.derivesFrom(defn.UnitClass) then
            report.error(em"`inline val` of type `Unit` is not supported.\n\nTo inline a `Unit` consider using `inline def`", rhs)
          else if tp.derivesFrom(defn.StringClass) || defn.ScalaValueClasses().exists(tp.derivesFrom(_)) then
            val pos = if tpt.span.isZeroExtent then rhs.srcPos else tpt.srcPos
            report.error(em"inline value must have a literal constant type", pos)
          else if tp.derivesFrom(defn.NullClass) then
            report.error(em"`inline val` with `null` is not supported.\n\nTo inline a `null` consider using `inline def`", rhs)
          else
            report.error(em"inline value must contain a literal constant value.\n\nTo inline more complex types consider using `inline def`", rhs)
  }

object InlineVals:
  val name: String = "inlineVals"
  val description: String = "check right hand-sides of an `inline val`s"
