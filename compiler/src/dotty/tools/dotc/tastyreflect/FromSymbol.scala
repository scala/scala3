package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

object FromSymbol {

  def definitionFromSym(sym: Symbol)(implicit ctx: Context): tpd.Tree = {
    assert(sym.exists)
    if (sym.is(Package)) packageDefFromSym(sym)
    else if (sym.isClass) classDef(sym.asClass)
    else if (sym.isType) typeDefFromSym(sym.asType)
    else if (sym.is(Method)) defDefFromSym(sym.asTerm)
    else valDefFromSym(sym.asTerm)
  }

  def packageDefFromSym(sym: Symbol)(implicit ctx: Context): PackageDefinition = PackageDefinitionImpl(sym)

  def classDef(cls: ClassSymbol)(implicit ctx: Context): tpd.TypeDef = cls.tree match {
    case tree: tpd.TypeDef => tree
    case tpd.EmptyTree =>
      val constrSym = cls.unforcedDecls.find(_.isPrimaryConstructor).orElse(
        // Dummy constructor for classes such as `<refinement>`
        ctx.newSymbol(cls, nme.CONSTRUCTOR, EmptyFlags, NoType)
      )
      val constr = tpd.DefDef(constrSym.asTerm)
      val parents = cls.classParents.map(tpd.TypeTree(_))
      val body = cls.unforcedDecls.filter(!_.isPrimaryConstructor).map(s => definitionFromSym(s))
      tpd.ClassDefWithParents(cls, constr, parents, body)
  }

  // TODO: this logic could be moved inside sym.tree

  def typeDefFromSym(sym: TypeSymbol)(implicit ctx: Context): tpd.TypeDef = sym.tree match {
    case tree: tpd.TypeDef => tree
    case tpd.EmptyTree => tpd.TypeDef(sym)
  }

  def defDefFromSym(sym: TermSymbol)(implicit ctx: Context): tpd.DefDef = sym.tree match {
    case tree: tpd.DefDef => tree
    case tpd.EmptyTree => tpd.DefDef(sym)
  }

  def valDefFromSym(sym: TermSymbol)(implicit ctx: Context): tpd.ValDef = sym.tree match {
    case tree: tpd.ValDef => tree
    case tpd.EmptyTree => tpd.ValDef(sym)
  }

}
