package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.MegaPhase._

/** Removes selects that would be compiled into GetStatic
 * otherwise backend needs to be aware that some qualifiers need to be dropped.
 * Similar transformation seems to be performed by flatten in nsc
 * @author Dmytro Petrashko
 */
class SelectStatic extends MiniPhase with IdentityDenotTransformer {
  import ast.tpd._

  override def phaseName: String = "selectStatic"

  override def transformSelect(tree: tpd.Select)(implicit ctx: Context): tpd.Tree = {
    val sym = tree.symbol
    def isStaticMember =
      (sym is Flags.Module) && sym.initial.maybeOwner.initial.isStaticOwner ||
      (sym is Flags.JavaStatic) ||
      sym.hasAnnotation(ctx.definitions.ScalaStaticAnnot)
    val isStaticRef = !sym.is(Package) && !sym.maybeOwner.is(Package) && isStaticMember
    val tree1 =
      if (isStaticRef && !tree.qualifier.symbol.is(JavaModule) && !tree.qualifier.isType)
        Block(List(tree.qualifier), ref(sym))
      else tree

    normalize(tree1)
  }

  private def normalize(t: Tree)(implicit ctx: Context) = t match {
    case Select(Block(stats, qual), nm) =>
      Block(stats, cpy.Select(t)(qual, nm))
    case Apply(Block(stats, qual), nm) =>
      Block(stats, Apply(qual, nm))
    case TypeApply(Block(stats, qual), nm) =>
      Block(stats, TypeApply(qual, nm))
    case Closure(env, Block(stats, qual), tpt) =>
      Block(stats, Closure(env, qual, tpt))
    case _ => t
  }

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = {
    normalize(tree)
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context): tpd.Tree = {
    normalize(tree)
  }

  override def transformClosure(tree: tpd.Closure)(implicit ctx: Context): tpd.Tree = {
    normalize(tree)
  }
}
