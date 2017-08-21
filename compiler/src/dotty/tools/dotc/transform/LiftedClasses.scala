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
class LiftedClasses extends MiniPhaseTransform with SymTransformer { thisTransformer =>

  override def phaseName = "liftedClasses"

  override def runsAfterGroupsOf: Set[Class[_ <: Phases.Phase]] = Set(classOf[Flatten])

  def transformSym(ref: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (needsRefresh(ref.symbol)) ref.copySymDenotation(name = refreshedName(ref.symbol))
    else ref

  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    if (needsRefresh(tree.symbol)) cpy.TypeDef(tree)(name = tree.symbol.name.asTypeName)
    else tree

  private def needsRefresh(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.isClass && !sym.is(Package) && sym.name.is(UniqueName)

  /* Makes a new unique name based on a unique name that was flatten */
  private def refreshedName(sym: Symbol)(implicit ctx: Context): TypeName = {
    // TODO: Refresh names not only based on their full name?
    //       Include package to distinguish <class>$anon from <pack>.<class>$anon
    val name = sym.name
    val newName = (name.firstPart.toString + str.NAME_JOIN + name.lastPart).toTermName

    var freshName = UniqueName.fresh(newName).toTypeName
    if (name.toSimpleName.endsWith(str.MODULE_SUFFIX))
      freshName = (freshName + str.MODULE_SUFFIX).toTypeName

    freshName
  }
}
