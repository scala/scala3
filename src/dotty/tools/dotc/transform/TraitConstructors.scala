package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.{SymTransformer, DenotTransformer}
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/***
 * Renames constructors in traits so that backend will call them with invokeInterface
 */
class TraitConstructors extends MiniPhaseTransform with SymTransformer {
  import dotty.tools.dotc.ast.tpd._
  def phaseName: String = "traitConstructors"

  def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    if (sym.isPrimaryConstructor && (sym.owner is Flags.Trait))
      sym.copySymDenotation(name = nme.IMPLCLASS_CONSTRUCTOR)
    else sym
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (sym.isPrimaryConstructor && (sym.owner is Flags.Trait))
      cpy.DefDef(tree)(name = nme.IMPLCLASS_CONSTRUCTOR)
    else tree
  }
}
