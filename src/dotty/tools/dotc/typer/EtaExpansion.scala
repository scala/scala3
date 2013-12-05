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

  private def lift(defs: mutable.ListBuffer[Tree], expr: Tree, prefix: String = "")(implicit ctx: Context): Tree =
    if (isIdempotentExpr(expr)) expr
    else {
      val name = ctx.freshName(prefix).toTermName
      val sym = ctx.newSymbol(ctx.owner, name, EmptyFlags, expr.tpe, coord = positionCoord(expr.pos))
      defs += ValDef(sym, expr)
      Ident(sym.valRef)
    }

  /** Lift out common part of tree taking part in an operator assignment such as
   *
   *     tree += expr
   */
  def liftAssigned(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Apply(fn, args) =>
      cpy.Apply(tree, lift(defs, fn), liftArgs(defs, fn.tpe, args))
    case Select(pre, name) =>
      cpy.Select(tree, lift(defs, pre), name)
    case _ =>
      tree
  }

  /** Lift a function argument, stripping any NamedArg wrapper */
  def liftArg(defs: mutable.ListBuffer[Tree], arg: Tree, prefix: String = "")(implicit ctx: Context): Tree = {
    val arg1 = arg match {
      case NamedArg(_, arg1) => arg1
      case arg => arg
    }
    lift(defs, arg1, prefix)
  }

  /** Lift arguments that are not-idempotent into ValDefs in buffer `defs`
   *  and replace by the idents of so created ValDefs.
   */
  def liftArgs(defs: mutable.ListBuffer[Tree], methType: Type, args: List[Tree])(implicit ctx: Context) =
    methType match {
      case MethodType(paramNames, paramTypes) =>
        (args, paramNames, paramTypes).zipped map { (arg, name, tp) =>
          if (tp isRef defn.ByNameParamClass) arg
          else liftArg(defs, arg, if (name contains '$') "" else name.toString + "$")
        }
      case _ =>
        args map (liftArg(defs, _))
    }

  /** Lift out function prefix and all arguments from application
   *
   *     pre.f(arg1, ..., argN)   becomes
   *
   *     val x0 = pre
   *     val x1 = arg1
   *     ...
   *     val xN = argN
   *     x0.f(x1, ..., xN)
   *
   *  But leave idempotent expressions alone.
   *
   */
  def liftApp(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Apply(fn, args) =>
      cpy.Apply(tree, liftApp(defs, fn), liftArgs(defs, fn.tpe, args))
    case TypeApply(fn, targs) =>
      cpy.TypeApply(tree, liftApp(defs, fn), targs)
    case Select(pre, name) if tpd.isIdempotentRef(tree) =>
      cpy.Select(tree, liftApp(defs, pre), name)
    case Block(stats, expr) =>
      liftApp(defs ++= stats, expr)
    case New(tpt) =>
      tree
    case _ =>
      lift(defs, tree)
  }

  /** Eta-expanding a tree means converting a method reference to a function value.
   *  @param    tree The tree to expand
   *  @param    wtp  The widened type of the tree, which is always a MethodType
   *  Let `wtp` be the method type
   *
   *         (x1: T1, ..., xn: Tn): R
   *
   *  and assume the lifted application of `tree` (@see liftApp) is
   *
   *         { val xs = es; expr }
   *
   *  Then the eta-expansion is
   *
   *         { val xs = es;
   *           { def $anonfun(x1: T1, ..., xn: Tn): T = expr; Closure($anonfun) }}
   */
  def etaExpand(tree: Tree, tpe: MethodType)(implicit ctx: Context): Tree = {
    def expand(lifted: Tree): Tree = {
      val meth = ctx.newSymbol(ctx.owner, nme.ANON_FUN, Synthetic, tpe, coord = tree.pos)
      Closure(meth, Apply(lifted, _))
    }
    val defs = new mutable.ListBuffer[Tree]
    val lifted = liftApp(defs, tree)
    wrapDefs(defs, expand(lifted))
  }

  def wrapDefs(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree =
    if (defs != null && defs.nonEmpty) tpd.Block(defs.toList, tree) else tree
}

  /** <p> not needed
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
