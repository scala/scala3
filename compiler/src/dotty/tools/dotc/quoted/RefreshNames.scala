package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.ContextRenamed
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds.{NumberedInfo, UniqueName}
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** Refreshes local names starting from the second use of the name. Intended for readability of the pretty printed code. */
class RefreshNames extends MiniPhase with SymTransformer {

  def phaseName: String = "RefreshNames"

  override def transformValDef(tree: tpd.ValDef)(implicit ctx: ContextRenamed): tpd.Tree =
    tpd.ValDef(tree.symbol.asTerm, tree.rhs)

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: ContextRenamed): tpd.Tree =
    tpd.DefDef(tree.symbol.asTerm, tree.rhs)

  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: ContextRenamed): tpd.Tree = {
    val newTypeDef = tpd.TypeDef(tree.symbol.asType)
    // keep rhs to keep `type T = ...` instead of `type T >: ... <: ...`
    cpy.TypeDef(newTypeDef)(rhs = tree.rhs)
  }

  def transformSym(ref: SymDenotation)(implicit ctx: ContextRenamed): SymDenotation = {
    if (ref.is(Package) || ref.isClass || ref.owner != ctx.owner || ref.is(Label) || ref.is(Param)) ref
    else {
      val newName = UniqueName.fresh(ref.symbol.name.toTermName)
      newName.info match {
        case info: NumberedInfo if info.num == 1 => ref // Keep the first reference as is to avoid renaming if the code has no duplicated names
        case _ => ref.copySymDenotation(name = if (ref.symbol.isType) newName.toTypeName else newName)
      }
    }
  }
}
