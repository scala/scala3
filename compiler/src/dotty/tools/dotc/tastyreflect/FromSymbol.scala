package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._

object FromSymbol {

  def definition(sym: Symbol)(implicit ctx: Context): tpd.Tree = {
    if (sym.is(Package)) packageDef(sym)
    else if (sym == defn.AnyClass) tpd.EmptyTree // FIXME
    else if (sym == defn.NothingClass) tpd.EmptyTree // FIXME
    else if (sym.isClass) classDef(sym.asClass)
    else if (sym.isType) typeDef(sym.asType)
    else if (sym.is(Method)) defDef(sym.asTerm)
    else valDef(sym.asTerm)
  }

  def packageDef(sym: Symbol)(implicit ctx: Context): PackageDefinition = PackageDefinitionImpl(sym)

  def classDef(cls: ClassSymbol)(implicit ctx: Context): tpd.Tree = {
    val constrSym = cls.unforcedDecls.find(_.isPrimaryConstructor)
    if (!constrSym.exists) return tpd.EmptyTree
    val constr = tpd.DefDef(constrSym.asTerm)
    val parents = cls.classParents.map(tpd.TypeTree(_))
    val body = cls.unforcedDecls.filter(!_.isPrimaryConstructor).map(s => definition(s))
    tpd.ClassDefWithParents(cls, constr, parents, body)
  }

  def typeDef(sym: TypeSymbol)(implicit ctx: Context): tpd.TypeDef = tpd.TypeDef(sym)

  def defDef(sym: TermSymbol)(implicit ctx: Context): tpd.DefDef = tpd.DefDef(sym)

  def valDef(sym: TermSymbol)(implicit ctx: Context): tpd.ValDef = tpd.ValDef(sym)

}
