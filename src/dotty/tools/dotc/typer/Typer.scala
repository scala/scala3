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

trait TyperContextOps { ctx: Context => }


class Typer extends Namer {

  import tpd._

  def typed(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = ???
  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = ???
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = ???

  type DefTyper[UT <: untpd.NameTree, T <: tpd.Tree] = (UT, NamedType) => Context => T

  def lateDef[UT <: untpd.NameTree, T <: tpd.Tree](defn: UT, op: DefTyper[UT, T])(implicit ctx: Context): T = {
    val sym = symOfUntypedTree(defn)
    sym.ensureCompleted()
    untypedTreeOfSym -= sym
    typedTreeOfSym remove sym match {
      case Some(tree) => tree.asInstanceOf[T]
      case None => op(defn, sym.symRef)(ctx)
    }
  }

  def aheadDef[UT <: untpd.NameTree, T <: tpd.Tree](defn: UT, op: DefTyper[UT, T])(implicit ctx: Context): T = {
    val sym = symOfUntypedTree(defn)
    val tree1 = op(defn, sym.symRef)(ctx)
    typedTreeOfSym(sym) = tree1
    tree1
  }

  def noDefTyper: DefTyper[untpd.NameTree, Nothing] = { (tdef, pt) => implicit ctx => ??? }

  val completeTypeDef: DefTyper[untpd.TypeDef, TypeDef] = { (tdef, pt) => implicit ctx =>
    val Trees.TypeDef(mods, name, tparams, rhs) = tdef
    val mods1 = typedModifiers(mods)
    val tparams1 = reEnterParams(tparams)
    val rhs1 = typedType(rhs)
    tdef.withType(pt).derivedTypeDef(mods1, name, tparams1, rhs1)
  }

  def typedTypedDef(tdef: untpd.TypeDef)(implicit ctx: Context): TypeDef =
    lateDef(tdef, completeTypeDef)

  def typedTptRhs(tpt: untpd.Tree, rhs: untpd.Tree)(implicit ctx: Context): (Tree, Tree) = {
    var tpt1: Tree = EmptyTree
    var rhs1: Tree = EmptyTree
    if (tpt.isEmpty) {
      rhs1 = typedExpr(rhs)
      tpt1 = tpt.withType(rhs1.tpe)
    } else {
      tpt1 = typedType(tpt)
      rhs1 = typedExpr(rhs, tpt1.tpe)
    }
    (tpt1, rhs1)
  }

  val completeValDef: DefTyper[untpd.ValDef, ValDef] = { (vdef, pt) => implicit ctx: Context =>
    val Trees.ValDef(mods, name, tpt, rhs) = vdef
    val mods1 = typedModifiers(mods)
    val (tpt1, rhs1) = typedTptRhs(tpt, rhs)
    vdef.withType(tpt1.tpe).derivedValDef(mods1, name, tpt1, rhs1)
  }

  def reEnterParams[UT <: untpd.NameTree, T <: tpd.Tree](params: List[UT])(implicit ctx: Context): List[T] = {
    for (param <- params) yield {
      val sym = symOfUntypedTree(param)
      ctx.enter(sym)
      lateDef(param, noDefTyper)
    }
  }

  val completeDefDef: DefTyper[untpd.DefDef, DefDef] = { (ddef, pt) => implicit ctx: Context =>
    val Trees.DefDef(mods, name, tparams, vparamss, tpt, rhs) = ddef
    val mods1 = typedModifiers(mods)
    val tparams1: List[TypeDef] = reEnterParams(tparams)
    val vparamss1: List[List[ValDef]] = vparamss.mapconserve(reEnterParams)
    val (tpt1, rhs1) = typedTptRhs(tpt, rhs)
    ddef.withType(tpt1.tpe).derivedDefDef(mods1, name, tparams1, vparamss1, tpt1, rhs1)
  }

  def typedImport(imp: untpd.Import, pt: Type)(implicit ctx: Context): Import = {
    val expr1 = typed(imp.expr)
    imp.withType(pt).derivedImport(expr1, imp.selectors)
  }

  def typedModifiers(mods: untpd.Modifiers): Modifiers = ???
}