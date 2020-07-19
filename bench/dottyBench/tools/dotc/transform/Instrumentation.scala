package dottyBench.tools.dotc
package transform

import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import MegaPhase._
import StdNames.nme
import Names.TermName
import Constants.Constant


/** The phase is enabled if a -Yinstrument-... option is set.
 *  If enabled, it counts the number of closures or allocations for each source position.
 *  It does this by generating a call to dottyBench.tools.dotc.util.Stats.doRecord.
 */
class Instrumentation extends MiniPhase { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "instrumentation"

  override def isEnabled(using Ctx) =
    ctx.settings.YinstrumentClosures.value ||
    ctx.settings.YinstrumentAllocations.value

  private var consName: TermName = _
  private var consEqName: TermName = _

  override def prepareForUnit(tree: Tree)(using Ctx, CState): Ctx = {
    consName = "::".toTermName
    consEqName = "+=".toTermName
    ctx
  }

  private def record(category: String, tree: Tree)(using Ctx, CState): Tree = {
    val key = Literal(Constant(s"$category${tree.sourcePos.show}"))
    ref(defn.Stats_doRecord).appliedTo(key, Literal(Constant(1)))
  }

  override def transformApply(tree: Apply)(using Ctx, CState): Tree = tree.fun match {
    case Select(nu: New, _) =>
      cpy.Block(tree)(record(i"alloc/${nu.tpe}@", tree) :: Nil, tree)
    case Select(_, name) if name == consName || name == consEqName =>
      cpy.Block(tree)(record("alloc/::", tree) :: Nil, tree)
    case _ =>
      tree
  }

  override def transformBlock(tree: Block)(using Ctx, CState): Block = tree.expr match {
    case _: Closure =>
      cpy.Block(tree)(record("closure/", tree) :: tree.stats, tree.expr)
    case _ =>
      tree
  }
}
