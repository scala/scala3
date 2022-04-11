package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd}
import Contexts._
import Types._
import Flags._
import Symbols._
import Names._
import NameKinds.UniqueName
import util.Spans._
import collection.mutable
import Trees._

/** A class that handles argument lifting. Argument lifting is needed in the following
 *  scenarios:
 *   - eta expansion
 *   - applications with default arguments
 *   - applications with out-of-order named arguments
 *  Lifting generally lifts impure expressions only, except in the case of possible
 *  default arguments, where we lift also complex pure expressions, since in that case
 *  arguments can be duplicated as arguments to default argument methods.
 */
abstract class Lifter {
  import tpd._

  /** Test indicating `expr` does not need lifting */
  def noLift(expr: Tree)(using Context): Boolean

  /** The corresponding lifter for pass-by-name arguments */
  protected def exprLifter: Lifter = NoLift

  /** The flags of a lifted definition */
  protected def liftedFlags: FlagSet = EmptyFlags

  /** The tree of a lifted definition */
  protected def liftedDef(sym: TermSymbol, rhs: Tree)(using Context): MemberDef = ValDef(sym, rhs)

  /** Is lifting performed on erased terms? */
  protected def isErased = false

  private def lift(defs: mutable.ListBuffer[Tree], expr: Tree, prefix: TermName = EmptyTermName)(using Context): Tree =
    if (noLift(expr)) expr
    else {
      val name = UniqueName.fresh(prefix)
      // don't instantiate here, as the type params could be further constrained, see tests/pos/pickleinf.scala
      var liftedType = expr.tpe.widen.deskolemized
      if (liftedFlags.is(Method)) liftedType = ExprType(liftedType)
      val lifted = newSymbol(ctx.owner, name, liftedFlags | Synthetic, liftedType, coord = spanCoord(expr.span))
      defs += liftedDef(lifted, expr)
        .withSpan(expr.span)
        .changeNonLocalOwners(lifted)
        .setDefTree
      ref(lifted.termRef).withSpan(expr.span.focus)
    }

  /** Lift out common part of lhs tree taking part in an operator assignment such as
   *
   *     lhs += expr
   */
  def liftAssigned(defs: mutable.ListBuffer[Tree], tree: Tree)(using Context): Tree = tree match {
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
  private def liftArg(defs: mutable.ListBuffer[Tree], arg: Tree, prefix: TermName = EmptyTermName)(using Context): Tree =
    arg match {
      case arg @ NamedArg(name, arg1) => cpy.NamedArg(arg)(name, lift(defs, arg1, prefix))
      case arg => lift(defs, arg, prefix)
    }

  /** Lift arguments that are not-idempotent into ValDefs in buffer `defs`
   *  and replace by the idents of so created ValDefs.
   */
  def liftArgs(defs: mutable.ListBuffer[Tree], methRef: Type, args: List[Tree])(using Context): List[Tree] =
    methRef.widen match {
      case mt: MethodType =>
        args.lazyZip(mt.paramNames).lazyZip(mt.paramInfos).map { (arg, name, tp) =>
          if tp.hasAnnotation(defn.InlineParamAnnot) then arg
          else
            val lifter = if (tp.isInstanceOf[ExprType]) exprLifter else this
            lifter.liftArg(defs, arg, if (name.firstPart contains '$') EmptyTermName else name)
        }
      case _ =>
        args.mapConserve(liftArg(defs, _))
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
  def liftApp(defs: mutable.ListBuffer[Tree], tree: Tree)(using Context): Tree = tree match {
    case Apply(fn, args) =>
      val fn1 = liftApp(defs, fn)
      val args1 = liftArgs(defs, fn.tpe, args)
      if isErased then untpd.cpy.Apply(tree)(fn1, args1).withType(tree.tpe) // application may be partial
      else cpy.Apply(tree)(fn1, args1)
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
   *  unless `pre` is idempotent.
   */
  def liftPrefix(defs: mutable.ListBuffer[Tree], tree: Tree)(using Context): Tree =
    if (isIdempotentExpr(tree)) tree else lift(defs, tree)
}

/** No lifting at all */
object NoLift extends Lifter {
  def noLift(expr: tpd.Tree)(using Context): Boolean = true
}

/** Lift all impure arguments */
class LiftImpure extends Lifter {
  def noLift(expr: tpd.Tree)(using Context): Boolean = tpd.isPureExpr(expr)
}
object LiftImpure extends LiftImpure

/** Lift all impure or complex arguments */
class LiftComplex extends Lifter {
  def noLift(expr: tpd.Tree)(using Context): Boolean = tpd.isPurePath(expr)
  override def exprLifter: Lifter = LiftToDefs
}
object LiftComplex extends LiftComplex

object LiftErased extends LiftComplex:
  override def isErased = true

/** Lift all impure or complex arguments to `def`s */
object LiftToDefs extends LiftComplex {
  override def liftedFlags: FlagSet = Method
  override def liftedDef(sym: TermSymbol, rhs: tpd.Tree)(using Context): tpd.DefDef = tpd.DefDef(sym, rhs)
}

/** Lifter for eta expansion */
object EtaExpansion extends LiftImpure {
  import tpd._

  /** Eta-expanding a tree means converting a method reference to a function value.
   *  @param    tree       The tree to expand
   *  @param    mt         The type of the method reference
   *  @param    xarity     The arity of the expected function type
   *  and assume the lifted application of `tree` (@see liftApp) is
   *
   *         { val xs = es; expr }
   *
   *  The result of the eta-expansion is either (1)
   *
   *         { val xs = es; (x1, ..., xn) => expr(x1, ..., xn) }
   *
   *  or (2)
   *
   *         { val xs = es; (x1: T1, ..., xn: Tn) => expr(x1, ..., xn) }
   *
   *  or (3)
   *
   *         { val xs = es; (x1: T1, ..., xn: Tn) => expr(x1, ..., xn) _ }
   *
   *  where `T1, ..., Tn` are the paremeter types of the expanded method.
   *  If `expr` has implicit function type, the arguments are passed with `given`.
   *  E.g. for (1):
   *
   *      { val xs = es; (x1, ..., xn) => expr(using x1, ..., xn) }
   *
   *  Case (3) applies if the method is curried, i.e. its result type is again a method
   *  type. Case (2) applies if the expected arity of the function type `xarity` differs
   *  from the number of parameters in `mt`. Case (1) applies if `mt` is uncurried
   *  and its number of parameters equals `xarity`. In this case we can always infer
   *  the parameter types later from the callee even if parameter types could not be
   *  inferred from the expected type. Hence, we lose nothing by omitting parameter types
   *  in the eta expansion. On the other hand omitting these parameters keeps the possibility
   *  open that different parameters are inferred from the expected type, so we keep
   *  more options open.
   *
   *  In each case, the result is an untyped tree, with `es` and `expr` as typed splices.
   *
   *    F[V](x) ==> (x => F[X])
   *
   *  Note: We allow eta expanding a method with a call by name parameter like
   *
   *    def m(x: => T): T
   *
   *  to a value of type (=> T) => T. This type cannot be written in source, since
   *  by-name types => T are not legal argument types.
   *
   *  It would be simpler to not allow to eta expand by-name methods. That was the rule
   *  initially, but at some point, the rule was dropped. Enforcing the restriction again
   *  now would break existing code. Allowing by-name parameters in function types seems to
   *  be OK. After elimByName they are all converted to regular function types anyway.
   *  But see comment on the `ExprType` case in function `prune` in class `ConstraintHandling`.
   */
  def etaExpand(tree: Tree, mt: MethodType, xarity: Int)(using Context): untpd.Tree = {
    import untpd._
    assert(!ctx.isAfterTyper || (ctx.phase eq ctx.base.inliningPhase), ctx.phase)
    val defs = new mutable.ListBuffer[tpd.Tree]
    val lifted: Tree = TypedSplice(liftApp(defs, tree))
    val isLastApplication = mt.resultType match {
      case rt: MethodType => rt.isImplicitMethod
      case _ => true
    }
    val paramTypes: List[Tree] =
      if (isLastApplication && mt.paramInfos.length == xarity) mt.paramInfos map (_ => TypeTree())
      else mt.paramInfos map TypeTree
    var paramFlag = Synthetic | Param
    if (mt.isContextualMethod) paramFlag |= Given
    else if (mt.isImplicitMethod) paramFlag |= Implicit
    val params = mt.paramNames.lazyZip(paramTypes).map((name, tpe) =>
      ValDef(name, tpe, EmptyTree).withFlags(paramFlag).withSpan(tree.span.startPos))
    var ids: List[Tree] = mt.paramNames map (name => Ident(name).withSpan(tree.span.startPos))
    if (mt.paramInfos.nonEmpty && mt.paramInfos.last.isRepeatedParam)
      ids = ids.init :+ repeated(ids.last)
    val body = Apply(lifted, ids)
    if (mt.isContextualMethod) body.setApplyKind(ApplyKind.Using)
    val fn =
      if (mt.isContextualMethod) new untpd.FunctionWithMods(params, body, Modifiers(Given))
      else if (mt.isImplicitMethod) new untpd.FunctionWithMods(params, body, Modifiers(Implicit))
      else untpd.Function(params, body)
    if (defs.nonEmpty) untpd.Block(defs.toList map (untpd.TypedSplice(_)), fn) else fn
  }
}
