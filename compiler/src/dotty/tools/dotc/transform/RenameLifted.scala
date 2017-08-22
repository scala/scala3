package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Renames lifted classes to local numbering scheme */
class RenameLifted extends MiniPhaseTransform with SymTransformer { thisTransformer =>

  override def phaseName = "renameLifted"

  override def runsAfterGroupsOf: Set[Class[_ <: Phases.Phase]] = Set(classOf[RestoreScopes])

  def transformSym(ref: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (needsRefresh(ref.symbol)) ref.copySymDenotation(name = refreshedName(ref.symbol))
    else ref

  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    if (needsRefresh(tree.symbol)) cpy.TypeDef(tree)(name = tree.symbol.name.asTypeName)
    else tree

  private def needsRefresh(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.isClass && sym.name.is(UniqueName)

  /* Makes a new unique name based on a unique name that was flatten */
  private def refreshedName(sym: Symbol)(implicit ctx: Context): TypeName = {
    val packName = sym.owner.fullName.toString
    val name = sym.name
    val newName = (packName + name.firstPart + str.NAME_JOIN + name.lastPart).toTermName

    var freshName = UniqueName.fresh(newName).toTypeName
    val prefix = if (sym.owner.isEmptyPackage) "$empty$" else packName
    freshName = freshName.toString.substring(prefix.length).toTypeName
    if (name.toSimpleName.endsWith(str.MODULE_SUFFIX))
      freshName = (freshName + str.MODULE_SUFFIX).toTypeName

    freshName
  }
}
