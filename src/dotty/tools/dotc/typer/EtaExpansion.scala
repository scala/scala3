package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import Contexts._
import Types._
import Flags._
import NameOps._
import Symbols._
import Decorators._
import Names._
import StdNames._
import Trees._
import util.Positions._
import collection.mutable

object EtaExpansion {

  import tpd._

  def lift(defs: mutable.ListBuffer[Tree], expr: Tree, prefix: String = "")(implicit ctx: Context): Tree =
    if (isIdempotentExpr(expr)) expr
    else {
      val name = ctx.freshName(prefix).toTermName
      val sym = ctx.newSymbol(ctx.owner, name, EmptyFlags, expr.tpe, coord = positionCoord(expr.pos))
      defs += ValDef(sym, expr)
      Ident(sym.valRef)
    }

  def liftArgs(defs: mutable.ListBuffer[Tree], methType: Type, args: List[Tree])(implicit ctx: Context) = {
    def toPrefix(name: Name) = if (name contains '$') "" else name.toString
    val paramInfos = methType match {
      case MethodType(paramNames, paramTypes) =>
        (paramNames, paramTypes).zipped map ((name, tp) =>
          (toPrefix(name), tp isRef defn.ByNameParamClass))
      case _ =>
        args map Function.const(("", false))
    }
    for ((arg, (prefix, isByName)) <- args zip paramInfos)
    yield
      if (isByName) arg else lift(defs, arg, prefix)
  }

  def liftApp(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Apply(fn, args) =>
      cpy.Apply(tree, liftApp(defs, fn), liftArgs(defs, fn.tpe, args))
    case TypeApply(fn, targs) =>
      cpy.TypeApply(tree, liftApp(defs, fn), targs)
    case Select(pre, name) =>
      cpy.Select(tree, lift(defs, pre), name)
    case Ident(name) =>
      lift(defs, tree)
    case Block(stats, expr) =>
      liftApp(defs ++= stats, expr)
    case _ =>
      tree
  }

  /** <p>
   *    Expand partial function applications of type `type`.
   *  </p><pre>
   *  p.f(es_1)...(es_n)
   *     ==>  {
   *            <b>private synthetic val</b> eta$f   = p.f   // if p is not stable
   *            ...
   *            <b>private synthetic val</b> eta$e_i = e_i    // if e_i is not stable
   *            ...
   *            (ps_1 => ... => ps_m => eta$f([es_1])...([es_m])(ps_1)...(ps_m))
   *          }</pre>
   *  <p>
   *    tree is already attributed
   *  </p>
  def etaExpandUntyped(tree: Tree)(implicit ctx: Context): untpd.Tree = { // kept as a reserve for now
    def expand(tree: Tree): untpd.Tree = tree.tpe match {
      case mt @ MethodType(paramNames, paramTypes) if !mt.isImplicit =>
        val paramsArgs: List[(untpd.ValDef, untpd.Tree)] =
          (paramNames, paramTypes).zipped.map { (name, tp) =>
            val droppedStarTpe = defn.underlyingOfRepeated(tp)
            val param = ValDef(
              Modifiers(Param), name,
              untpd.TypedSplice(TypeTree(droppedStarTpe)), untpd.EmptyTree)
            var arg: untpd.Tree = Ident(name)
            if (defn.isRepeatedParam(tp))
              arg = Typed(arg, Ident(tpnme.WILDCARD_STAR))
            (param, arg)
          }
        val (params, args) = paramsArgs.unzip
        untpd.Function(params, Apply(untpd.TypedSplice(tree), args))
    }

    val defs = new mutable.ListBuffer[Tree]
    val tree1 = liftApp(defs, tree)
    Block(defs.toList map untpd.TypedSplice, expand(tree1))
  }
   */

  def etaExpand(tree: Tree, tpe: MethodType)(implicit ctx: Context): Tree = {
    def expand(tree: Tree): Tree = {
      val meth = ctx.newSymbol(ctx.owner, nme.ANON_FUN, Synthetic, tpe, coord = tree.pos)
      Closure(meth, Apply(tree, _))
    }
    val defs = new mutable.ListBuffer[Tree]
    val tree1 = liftApp(defs, tree)
    Block(defs.toList, expand(tree1))
  }
}