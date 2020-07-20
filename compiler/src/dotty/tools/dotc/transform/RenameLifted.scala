package dotty.tools.dotc.transform

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** Renames lifted classes to local numbering scheme */
class RenameLifted extends MiniPhase with SymTransformer {

  override def phaseName: String = "renameLifted"

  // Not clear why this should run after restoreScopes
  // override def runsAfterGroupsOf = Set(RestoreScopes.name)

  def transformSym(ref: SymDenotation)(using Context): SymDenotation =
    if (needsRefresh(ref.symbol)) ref.copySymDenotation(name = refreshedName(ref.symbol))
    else ref

  /** If the name of the symbol with a unique name needs to be refreshed
   *    - if it is a lifted class
   *    - if it is a lifted method
   */
  private def needsRefresh(sym: Symbol)(using Context): Boolean =
    (sym.isClass || sym.isOneOf(Private | Method | JavaStatic)) && sym.name.is(UniqueName)

  /** Refreshes the number of the name based on the full name of the symbol */
  private def refreshedName(sym: Symbol)(using Context): Name = {
    def rewriteUnique: PartialFunction[Name, Name] = {
      case name: DerivedName if name.info.kind == UniqueName =>
        val fullName = (sym.owner.fullName.toString + name.underlying).toTermName
        val freshName = UniqueName.fresh(fullName)
        val info = freshName.asInstanceOf[DerivedName].info
        DerivedName(name.underlying.replace(rewriteUnique), info)
      case DerivedName(underlying, info: QualifiedInfo) =>
        underlying.replace(rewriteUnique).derived(info)
    }

    sym.name.replace(rewriteUnique)
  }
}
