package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** This phase removes unused declarations of `def`s and `val`s (except for parameters).
 *  It assumes that:
 *    - Unused defs and vals are not used
 *    - There are no unused parameter declarations
 *
 *  `@unused def f(...) = ...` and  `@unused val x = ...` are removed
 */
class UnusedDecls extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "unusedDecls"

  override def runsAfterGroupsOf = Set(
    classOf[UnusedParams], // ensure no unused parameters declarations
    classOf[UnusedRefs] // ensures declarations are not used
  )

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: ValOrDefDef => assert(!tree.symbol.is(Unused))
    case _ =>
  }


  /* Tree transform */

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol.is(Unused)) EmptyTree
    else tree

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol.is(Unused)) EmptyTree
    else tree


  /* Info transform */

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp: ClassInfo =>
      if (tp.classSymbol.is(JavaDefined) || !tp.decls.iterator.exists(_.is(Unused))) tp
      else tp.derivedClassInfo(decls = tp.decls.filteredScope(!_.is(Unused)))
    case _ => tp
  }

}
