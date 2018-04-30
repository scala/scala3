package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._


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

  def packageDef(sym: Symbol)(implicit ctx: Context): tpd.PackageDef =
    tpd.PackageDef(tpd.Ident(sym.typeRef), Nil)

  def classDef(cls: ClassSymbol)(implicit ctx: Context): tpd.Tree = {
    val constr = tpd.DefDef(cls.unforcedDecls.find(_.isPrimaryConstructor).asTerm)
    val body = cls.unforcedDecls.filter(!_.isPrimaryConstructor).map(s => definition(s))
    val superArgs = Nil // TODO
    tpd.ClassDef(cls, constr, body, superArgs)
  }

  def typeDef(sym: TypeSymbol)(implicit ctx: Context): tpd.TypeDef = tpd.TypeDef(sym)

  def defDef(sym: TermSymbol)(implicit ctx: Context): tpd.DefDef = tpd.DefDef(sym)

  def valDef(sym: TermSymbol)(implicit ctx: Context): tpd.ValDef = tpd.ValDef(sym)

}
