package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** This phase removes erased declarations of val`s (except for parameters).
 *
 *  `erased val x = ...` are removed
 */
class ErasedDecls extends MiniPhase with InfoTransformer {
  import tpd._

  override def phaseName: String = "erasedDecls"

  override def runsAfterGroupsOf = Set(
    PatternMatcher.name // Make sure pattern match errors are emitted
  )

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: ValOrDefDef if !tree.symbol.is(Param) => assert(!tree.symbol.is(Erased, butNot = Param))
    case _ =>
  }


  /* Tree transform */

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = transformValOrDefDef(tree)
  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = transformValOrDefDef(tree)

  private def transformValOrDefDef(tree: ValOrDefDef)(implicit ctx: Context): Tree =
    if (tree.symbol.is(Erased, butNot = Param)) EmptyTree else tree


  /* Info transform */

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp: ClassInfo => tp.derivedClassInfo(decls = tp.decls.filteredScope(!_.is(Erased)))
    case _ => tp
  }

  override protected def mayChange(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.isClass && !sym.is(JavaDefined) && !sym.is(Scala2x)
}
