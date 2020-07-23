package dotty.tools.dotc
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
import Names._
import Constants.Constant


/** The phase is enabled if a -Yinstrument-... option is set.
 *  If enabled, it counts the number of closures or allocations for each source position.
 *  It does this by generating a call to dotty.tools.dotc.util.Stats.doRecord.
 */
class Instrumentation extends MiniPhase { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "instrumentation"

  override def isEnabled(using Context) =
    ctx.settings.YinstrumentClosures.value ||
    ctx.settings.YinstrumentAllocations.value

  private val namesOfInterest = List("::", "+=", "toString")
  private var namesToRecord: Set[Name] = _

  private var consName: TermName = _
  private var consEqName: TermName = _

  override def prepareForUnit(tree: Tree)(using Context): Context =
    namesToRecord = namesOfInterest.map(_.toTermName).toSet
    ctx

  private def record(category: String, tree: Tree)(using Context): Tree = {
    val key = Literal(Constant(s"$category${tree.sourcePos.show}"))
    ref(defn.Stats_doRecord).appliedTo(key, Literal(Constant(1)))
  }

  override def transformApply(tree: Apply)(using Context): Tree = tree.fun match {
    case Select(nu: New, _) =>
      cpy.Block(tree)(record(i"alloc/${nu.tpe}@", tree) :: Nil, tree)
    case Select(_, name) if namesToRecord.contains(name) =>
      cpy.Block(tree)(record(i"alloc/$name", tree) :: Nil, tree)
    case _ =>
      tree
  }

  override def transformBlock(tree: Block)(using Context): Block = tree.expr match {
    case _: Closure =>
      cpy.Block(tree)(record("closure/", tree) :: tree.stats, tree.expr)
    case _ =>
      tree
  }
}
