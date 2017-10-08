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
import NameKinds.UniqueName
import Trees._
import Inferencing._
import util.Positions._
import collection.mutable

object EtaExpansion {

  import tpd._

  private def lift(defs: mutable.ListBuffer[Tree], expr: Tree, prefix: TermName = EmptyTermName)(implicit ctx: Context): Tree =
    if (isPureExpr(expr)) expr
    else {
      val name = UniqueName.fresh(prefix)
      val liftedType = fullyDefinedType(expr.tpe.widen, "lifted expression", expr.pos)
      val sym = ctx.newSymbol(ctx.owner, name, EmptyFlags, liftedType, coord = positionCoord(expr.pos))
      defs += ValDef(sym, expr)
      ref(sym.termRef)
    }

  /** Lift out common part of lhs tree taking part in an operator assignment such as
   *
   *     lhs += expr
   */
  def liftAssigned(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Apply(MaybePoly(fn @ Select(pre, name), targs), args) =>
      cpy.Apply(tree)(
        cpy.Select(fn)(
          lift(defs, pre), name).appliedToTypeTrees(targs),
          liftArgs(defs, fn.tpe, args))
    case Select(pre, name) =>
      cpy.Select(tree)(lift(defs, pre), name)
    case _ =>
      tree
  }

  /** Lift a function argument, stripping any NamedArg wrapper */
  def liftArg(defs: mutable.ListBuffer[Tree], arg: Tree, prefix: TermName = EmptyTermName)(implicit ctx: Context): Tree =
    arg match {
      case arg @ NamedArg(name, arg1) => cpy.NamedArg(arg)(name, lift(defs, arg1, prefix))
      case arg => lift(defs, arg, prefix)
    }

  /** Lift arguments that are not-idempotent into ValDefs in buffer `defs`
   *  and replace by the idents of so created ValDefs.
   */
  def liftArgs(defs: mutable.ListBuffer[Tree], methRef: Type, args: List[Tree])(implicit ctx: Context) =
    methRef.widen match {
      case mt: MethodType =>
        (args, mt.paramNames, mt.paramInfos).zipped map { (arg, name, tp) =>
          if (tp.isInstanceOf[ExprType]) arg
          else liftArg(defs, arg, if (name.firstPart contains '$') EmptyTermName else name)
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
   *  But leave pure expressions alone.
   *
   */
  def liftApp(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Apply(fn, args) =>
      cpy.Apply(tree)(liftApp(defs, fn), liftArgs(defs, fn.tpe, args))
    case TypeApply(fn, targs) =>
      cpy.TypeApply(tree)(liftApp(defs, fn), targs)
    case Select(pre, name) if isPureRef(tree) =>
      cpy.Select(tree)(liftPrefix(defs, pre), name)
    case Block(stats, expr) =>
      liftApp(defs ++= stats, expr)
    case New(tpt) =>
      tree
    case _ =>
      lift(defs, tree)
  }

  /** Lift prefix `pre` of an application `pre.f(...)` to
   *
   *     val x0 = pre
   *     x0.f(...)
   *
   *  unless `pre` is a `New` or `pre` is idempotent.
   */
  def liftPrefix(defs: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context): Tree = tree match {
    case New(_) => tree
    case _ => if (isIdempotentExpr(tree)) tree else lift(defs, tree)
  }

  /** Eta-expanding a tree means converting a method reference to a function value.
   *  @param    tree       The tree to expand
   *  @param    mt         The type of the method reference
   *  @param    xarity     The arity of the expected function type
   *  and assume the lifted application of `tree` (@see liftApp) is
   *
   *         { val xs = es; expr }
   *
   *  If xarity matches the number of parameters in `mt`, the eta-expansion is
   *
   *         { val xs = es; (x1, ..., xn) => expr(x1, ..., xn) }
   *
   * Note that the function value's parameters are untyped, hence the type will
   * be supplied by the environment (or if missing be supplied by the target
   * method as a fallback). On the other hand, if `xarity` is different from
   * the number of parameters in `mt`, then we cannot propagate parameter types
   * from the expected type, and we fallback to using the method's original
   * parameter types instead.
   *
   * In either case, the result is an untyped tree, with `es` and `expr` as typed splices.
   */
  def etaExpand(tree: Tree, mt: MethodType, xarity: Int)(implicit ctx: Context): untpd.Tree = {
    import untpd._
    assert(!ctx.isAfterTyper)
    val defs = new mutable.ListBuffer[tpd.Tree]
    val lifted: Tree = TypedSplice(liftApp(defs, tree))
    val paramTypes: List[Tree] =
      if (mt.paramInfos.length == xarity) mt.paramInfos map (_ => TypeTree())
      else mt.paramInfos map TypeTree
    val params = (mt.paramNames, paramTypes).zipped.map((name, tpe) =>
      ValDef(name, tpe, EmptyTree).withFlags(Synthetic | Param).withPos(tree.pos.startPos))
    var ids: List[Tree] = mt.paramNames map (name => Ident(name).withPos(tree.pos.startPos))
    if (mt.paramInfos.nonEmpty && mt.paramInfos.last.isRepeatedParam)
      ids = ids.init :+ repeated(ids.last)
    var body: Tree = Apply(lifted, ids)
    mt.resultType match {
      case rt: MethodType if !rt.isImplicitMethod => body = PostfixOp(body, Ident(nme.WILDCARD))
      case _ =>
    }
    val fn = untpd.Function(params, body)
    if (defs.nonEmpty) untpd.Block(defs.toList map (untpd.TypedSplice(_)), fn) else fn
  }
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
