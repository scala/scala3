package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import util.Positions._
import util.SourcePosition
import collection.mutable
import language.implicitConversions
import desugar.Mode

trait TyperContextOps { ctx: Context => }


class Typer extends Namer {

  import tpd._

  def typedImport(imp: untpd.Import, pt: Type)(implicit ctx: Context): Import = {
    val expr1 = typed(imp.expr)
    imp.withType(pt).derivedImport(expr1, imp.selectors)
  }

  def typedModifiers(mods: untpd.Modifiers): Modifiers = ???

  def typedValDef(defn: untpd.ValDef, sym: Symbol)(implicit ctx: Context) = {
    val Trees.ValDef(mods, name, tpt, rhs) = defn
    val mods1 = typedModifiers(mods)
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    val pt = if (sym.exists) sym.symRef else NoType
    defn.withType(pt).derivedValDef(mods1, name, tpt1, rhs1)
  }

  def typedDefDef(defn: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
    val Trees.DefDef(mods, name, tparams, vparamss, tpt, rhs) = defn
    val mods1 = typedModifiers(mods)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val vparamss1 = vparamss.mapconserve(_ mapconserve (typed(_).asInstanceOf[ValDef]))
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    defn.withType(sym.symRef).derivedDefDef(mods1, name, tparams1, vparamss1, tpt1, rhs1)
  }

  def typedTypeDef(defn: untpd.TypeDef, sym: Symbol)(implicit ctx: Context): TypeDef = {
    val Trees.TypeDef(mods, name, tparams, rhs) = defn
    val mods1 = typedModifiers(mods)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val rhs1 = typedType(rhs)
    defn.withType(sym.symRef).derivedTypeDef(mods1, name, tparams1, rhs1)
  }

  def typedClassDef(defn: untpd.ClassDef, cls: ClassSymbol)(implicit ctx: Context) = {
    val Trees.ClassDef(mods, name, impl @ Trees.Template(constr, parents, self, body)) = defn
    val mods1 = typedModifiers(mods)
    val constr1 = typed(constr)
    val parents1 = parents mapconserve (typed(_))
    val self1 = typed(self)
    ???
  }

  def typedMemberDef(defn: untpd.MemberDef, sym: Symbol)(implicit ctx: Context) = {
    sym.ensureCompleted()
    def localContext = ctx.fresh.withOwner(sym)
    defn match {
      case defn: untpd.ValDef =>
        typedValDef(defn, sym)(localContext)
      case defn: untpd.DefDef =>
        val typer1 = nestedTyper.remove(sym).get
        typer1.typedDefDef(defn, sym)(localContext.withScope(typer1.scope))
      case defn: untpd.TypeDef =>
        typedTypeDef(defn, sym)(localContext.withNewScope)
      case defn: untpd.ClassDef =>
        typedClassDef(defn, sym.asClass)(localContext)
    }
  }

  def typed(tree: untpd.Tree, mode: Mode.Value = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    typedTree get tree match {
      case Some(tree1) => tree1
      case none => tree match {
        case defn: untpd.MemberDef =>
          typedMemberDef(defn, symOfTree(defn))
        case imp: untpd.Import =>
          typedImport(imp, symOfTree(imp).symRef)
        case tree: untpd.TypeTree =>
          if (!tree.isEmpty) typed(tree.original, Mode.Type, pt)
          else {
            assert(pt != WildcardType)
            tree.withType(pt)
          }
        case untpd.EmptyTree =>
          tpd.EmptyTree
      }
    }
  }

  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, Mode.Expr, pt)
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, Mode.Type, pt)

}