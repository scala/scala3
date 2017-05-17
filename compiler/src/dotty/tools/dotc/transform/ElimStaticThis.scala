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

/** Replace This references to module classes in static methods by global identifiers to the
 *  corresponding modules.
 */
class ElimStaticThis extends MiniPhaseTransform {
  import ast.tpd._
  def phaseName: String = "elimStaticThis"

  override def transformThis(tree: This)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (!tree.symbol.is(Package) && ctx.owner.enclosingMethod.is(JavaStatic)) {
      assert(tree.symbol.is(ModuleClass))
      ref(tree.symbol.sourceModule)
    }
    else tree

  override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (ctx.owner.enclosingMethod.is(JavaStatic)) {
      tree.tpe match {
        case TermRef(thiz: ThisType, _) if thiz.cls.is(ModuleClass, JavaDefined) =>
          ref(thiz.cls.sourceModule).select(tree.symbol)
        case TermRef(thiz: ThisType, _) =>
          assert(tree.symbol.is(Flags.JavaStatic) || thiz.cls.is(JavaDefined))
          tree
        case _ => tree
      }
    }
    else tree
  }
}
