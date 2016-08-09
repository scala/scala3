package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.TreeTransforms._

/** Removes selects that would be compiled into GetStatic
 * otherwise backend needs to be aware that some qualifiers need to be dropped.
 * Similar transformation seems to be performed by flatten in nsc
 *
 * @author Dmytro Petrashko
 */
class SelectStatic extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "selectStatic"
  private val isPackage = FlagConjunction(PackageCreationFlags.bits)

  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (!sym.is(isPackage) && !sym.maybeOwner.is(isPackage) &&
      (
         ((sym is Flags.Module) && sym.maybeOwner.isStaticOwner) ||
         (sym is Flags.JavaStatic) ||
         (sym.maybeOwner is Flags.ImplClass) ||
         sym.hasAnnotation(ctx.definitions.ScalaStaticAnnot)
        )
    )
      if (!tree.qualifier.symbol.is(JavaModule))
        Block(List(tree.qualifier), ref(sym))
      else tree
    else tree
  }
}
