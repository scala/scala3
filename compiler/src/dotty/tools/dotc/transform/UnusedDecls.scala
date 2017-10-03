package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** This phase removes unused declarations of val`s (except for parameters).
 *
 *  `unused val x = ...` are removed
 */
class UnusedDecls extends MiniPhase with InfoTransformer {
  import tpd._

  override def phaseName: String = "unusedDecls"

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(
    classOf[PatternMatcher] // Make sure pattern match errors are emitted
  )

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: ValOrDefDef if !tree.symbol.is(Param) => assert(!tree.symbol.is(Unused, butNot = Param))
    case _ =>
  }


  /* Tree transform */

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    if (tree.symbol.is(Unused, butNot = Param)) EmptyTree else tree


  /* Info transform */

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp: ClassInfo =>
      if (tp.classSymbol.is(JavaDefined) || !tp.decls.iterator.exists(_.is(Unused))) tp
      else tp.derivedClassInfo(decls = tp.decls.filteredScope(!_.is(Unused)))
    case _ => tp
  }
}
