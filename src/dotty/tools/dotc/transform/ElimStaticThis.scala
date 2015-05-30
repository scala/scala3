package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Flags._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import dotty.tools.dotc.core.Types.{ThisType, TermRef}
import Phases.Phase

/** Replace This references to module classes  in static methods by global identifiers to the
 *  corresponding modules.
 */
class ElimStaticThis extends MiniPhaseTransform {
  import ast.tpd._
  def phaseName: String = "elimStaticThis"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Flatten])

  override def transformThis(tree: This)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (!tree.symbol.is(Package) && ctx.owner.enclosingMethod.is(JavaStatic)) {
      assert(tree.symbol.is(ModuleClass))
      ref(tree.symbol.sourceModule)
    }
    else tree

  override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val meth = ctx.owner.enclosingMethod
    // We cannot use meth.enclosingClass because it skips other static classes,
    // so instead we require this phase to run after Flatten and use meth.owner
    if (meth.is(JavaStatic) && meth.owner.is(ModuleClass)) {
      tree.tpe match {
        case TermRef(thiz: ThisType, _) if (thiz.underlying.typeSymbol == meth.owner) =>
          ref(thiz.underlying.typeSymbol.sourceModule).select(tree.symbol)
        case _ => tree
      }
    }
    else tree
  }
}
