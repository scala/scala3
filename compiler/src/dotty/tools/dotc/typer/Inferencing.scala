package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Symbols._
import Trees._
import ProtoTypes._
import NameKinds.UniqueName
import util.Spans._
import util.{Stats, SimpleIdentityMap}
import Decorators._
import config.Printers.{gadts, typr}
import annotation.tailrec
import reporting._
import collection.mutable

import scala.annotation.internal.sharable

object Inferencing {

  import tpd._

  /** Is type fully defined, meaning the type does not contain wildcard types
   *  or uninstantiated type variables. As a side effect, this will minimize
   *  any uninstantiated type variables, according to the given force degree,
   *  but only if the overall result of `isFullyDefined` is `true`.
   *  Variables that are successfully minimized do not count as uninstantiated.
   */
  def isFullyDefined(tp: Type, force: ForceDegree.Value)(implicit ctx: Context): Boolean = {
    val nestedCtx = ctx.fresh.setNewTyperState()
    val result = new IsFullyDefinedAccumulator(force)(nestedCtx).process(tp)
    if (result) nestedCtx.typerState.commit()
    result
  }

  /** The fully defined type, where all type variables are forced.
   *  Throws an error if type contains wildcards.
   */
  def fullyDefinedType(tp: Type, what: String, span: Span)(implicit ctx: Context): Type =
    if (isFullyDefined(tp, ForceDegree.all)) tp
    else throw new Error(i"internal error: type of $what $tp is not fully defined, pos = $span") // !!! DEBUG


  /** Instantiate selected type variables `tvars` in type `tp` */
  def instantiateSelected(tp: Type, tvars: List[Type])(implicit ctx: Context): Unit =
    if (tvars.nonEmpty)
      new IsFullyDefinedAccumulator(new ForceDegree.Value(tvars.contains, minimizeAll = true)).process(tp)

  /** Instantiate any type variables in `tp` whose bounds contain a reference to
   *  one of the parameters in `tparams` or `vparamss`.
   */
  def instantiateDependent(tp: Type, tparams: List[Symbol], vparamss: List[List[Symbol]])(implicit ctx: Context): Unit = {
    val dependentVars = new TypeAccumulator[Set[TypeVar]] {
      lazy val params = (tparams :: vparamss).flatten
      def apply(tvars: Set[TypeVar], tp: Type) = tp match {
        case tp: TypeVar
        if !tp.isInstantiated &&
            ctx.typeComparer.bounds(tp.origin)
              .namedPartsWith(ref => params.contains(ref.symbol))
              .nonEmpty =>
          tvars + tp
        case _ =>
          foldOver(tvars, tp)
      }
    }
    val depVars = dependentVars(Set(), tp)
    if (depVars.nonEmpty) instantiateSelected(tp, depVars.toList)
  }

  /** The accumulator which forces type variables using the policy encoded in `force`
   *  and returns whether the type is fully defined. The direction in which
   *  a type variable is instantiated is determined as follows:
   *   1. T is minimized if the constraint over T is only from below (i.e.
   *      constrained lower bound != given lower bound and
   *      constrained upper bound == given upper bound).
   *   2. T is maximized if the constraint over T is only from above (i.e.
   *      constrained upper bound != given upper bound and
   *      constrained lower bound == given lower bound).
   *  If (1) and (2) do not apply:
   *   3. T is minimized if forceDegree is minimizeAll.
   *   4. Otherwise, T is maximized if it appears only contravariantly in the given type,
   *      or if forceDegree is `noBottom` and T's minimized value is a bottom type.
   *   5. Otherwise, T is minimized.
   *
   *  The instantiation is done in two phases:
   *  1st Phase: Try to instantiate minimizable type variables to
   *  their lower bound. Record whether successful.
   *  2nd Phase: If first phase was successful, instantiate all remaining type variables
   *  to their upper bound.
   */
  private class IsFullyDefinedAccumulator(force: ForceDegree.Value)(implicit ctx: Context) extends TypeAccumulator[Boolean] {

    private def instantiate(tvar: TypeVar, fromBelow: Boolean): Type = {
      val inst = tvar.instantiate(fromBelow)
      typr.println(i"forced instantiation of ${tvar.origin} = $inst")
      inst
    }

    private[this] var toMaximize: Boolean = false

    def apply(x: Boolean, tp: Type): Boolean = tp.dealias match {
      case _: WildcardType | _: ProtoType =>
        false
      case tvar: TypeVar
      if !tvar.isInstantiated && ctx.typerState.constraint.contains(tvar) =>
        force.appliesTo(tvar) && {
          val direction = instDirection(tvar.origin)
          def avoidBottom =
            !force.allowBottom &&
            defn.isBottomType(ctx.typeComparer.approximation(tvar.origin, fromBelow = true))
          def preferMin = force.minimizeAll || variance >= 0 && !avoidBottom
          if (direction != 0) instantiate(tvar, direction < 0)
          else if (preferMin) instantiate(tvar, fromBelow = true)
          else toMaximize = true
          foldOver(x, tvar)
        }
      case tp =>
        foldOver(x, tp)
    }

    private class UpperInstantiator(implicit ctx: Context) extends TypeAccumulator[Unit] {
      def apply(x: Unit, tp: Type): Unit = {
        tp match {
          case tvar: TypeVar if !tvar.isInstantiated =>
            instantiate(tvar, fromBelow = false)
          case _ =>
        }
        foldOver(x, tp)
      }
    }

    def process(tp: Type): Boolean = {
      val res = apply(true, tp)
      if (res && toMaximize) new UpperInstantiator().apply((), tp)
      res
    }
  }

  /** If `tree` has a type lambda type, infer its type parameters by comparing with expected type `pt` */
  def inferTypeParams(tree: Tree, pt: Type)(implicit ctx: Context): Tree = tree.tpe match {
    case tl: TypeLambda =>
      val (tl1, tvars) = constrained(tl, tree)
      var tree1 = AppliedTypeTree(tree.withType(tl1), tvars)
      tree1.tpe <:< pt
      fullyDefinedType(tree1.tpe, "template parent", tree.span)
      tree1
    case _ =>
      tree
  }

  def isSkolemFree(tp: Type)(implicit ctx: Context): Boolean =
    !tp.existsPart(_.isInstanceOf[SkolemType])

  /** Derive information about a pattern type by comparing it with some variant of the
   *  static scrutinee type. We have the following situation in case of a (dynamic) pattern match:
   *
   *       StaticScrutineeType           PatternType
   *                         \            /
   *                      DynamicScrutineeType
   *
   *  If `PatternType` is not a subtype of `StaticScrutineeType, there's no information to be gained.
   *  Now let's say we can prove that `PatternType <: StaticScrutineeType`.
   *
   *            StaticScrutineeType
   *                  |         \
   *                  |          \
   *                  |           \
   *                  |            PatternType
   *                  |          /
   *               DynamicScrutineeType
   *
   *  What can we say about the relationship of parameter types between `PatternType` and
   *  `DynamicScrutineeType`?
   *
   *   - If `DynamicScrutineeType` refines the type parameters of `StaticScrutineeType`
   *     in the same way as `PatternType` ("invariant refinement"), the subtype test
   *     `PatternType <:< StaticScrutineeType` tells us all we need to know.
   *   - Otherwise, if variant refinement is a possibility we can only make predictions
   *     about invariant parameters of `StaticScrutineeType`. Hence we do a subtype test
   *     where `PatternType <: widenVariantParams(StaticScrutineeType)`, where `widenVariantParams`
   *     replaces all type argument of variant parameters with empty bounds.
   *
   *  Invariant refinement can be assumed if `PatternType`'s class(es) are final or
   *  case classes (because of `RefChecks#checkCaseClassInheritanceInvariant`).
   *
   *  TODO: Update so that GADT symbols can be variant, and we special case final class types in patterns
   */
  def constrainPatternType(tp: Type, pt: Type)(implicit ctx: Context): Boolean = {
    def refinementIsInvariant(tp: Type): Boolean = tp match {
      case tp: ClassInfo => tp.cls.is(Final) || tp.cls.is(Case)
      case tp: TypeProxy => refinementIsInvariant(tp.underlying)
      case tp: AndType => refinementIsInvariant(tp.tp1) && refinementIsInvariant(tp.tp2)
      case tp: OrType => refinementIsInvariant(tp.tp1) && refinementIsInvariant(tp.tp2)
      case _ => false
    }

    def widenVariantParams = new TypeMap {
      def apply(tp: Type) = mapOver(tp) match {
        case tp @ AppliedType(tycon, args) =>
          val args1 = args.zipWithConserve(tycon.typeParams)((arg, tparam) =>
            if (tparam.paramVariance != 0) TypeBounds.empty else arg
          )
          tp.derivedAppliedType(tycon, args1)
        case tp =>
          tp
      }
    }

    val widePt = if (ctx.scala2Mode || refinementIsInvariant(tp)) pt else widenVariantParams(pt)
    trace(i"constraining pattern type $tp <:< $widePt", gadts, res => s"$res\n${ctx.gadt.debugBoundsDescription}") {
      tp <:< widePt
    }
  }

  /** The list of uninstantiated type variables bound by some prefix of type `T` which
   *  occur in at least one formal parameter type of a prefix application.
   *  Considered prefixes are:
   *    - The function `f` of an application node `f(e1, .., en)`
   *    - The function `f` of a type application node `f[T1, ..., Tn]`
   *    - The prefix `p` of a selection `p.f`.
   *    - The result expression `e` of a block `{s1; .. sn; e}`.
   */
  def tvarsInParams(tree: Tree, locked: TypeVars)(implicit ctx: Context): List[TypeVar] = {
    @tailrec def boundVars(tree: Tree, acc: List[TypeVar]): List[TypeVar] = tree match {
      case Apply(fn, _) => boundVars(fn, acc)
      case TypeApply(fn, targs) =>
        val tvars = targs.filter(_.isInstanceOf[TypeVarBinder[_]]).tpes.collect {
          case tvar: TypeVar
          if !tvar.isInstantiated &&
             ctx.typerState.ownedVars.contains(tvar) &&
             !locked.contains(tvar) => tvar
        }
        boundVars(fn, acc ::: tvars)
      case Select(pre, _) => boundVars(pre, acc)
      case Block(_, expr) => boundVars(expr, acc)
      case _ => acc
    }
    @tailrec def occurring(tree: Tree, toTest: List[TypeVar], acc: List[TypeVar]): List[TypeVar] =
      if (toTest.isEmpty) acc
      else tree match {
        case Apply(fn, _) =>
          fn.tpe.widen match {
            case mtp: MethodType =>
              val (occ, nocc) = toTest.partition(tvar => mtp.paramInfos.exists(tvar.occursIn))
              occurring(fn, nocc, occ ::: acc)
            case _ =>
              occurring(fn, toTest, acc)
          }
        case TypeApply(fn, targs) => occurring(fn, toTest, acc)
        case Select(pre, _) => occurring(pre, toTest, acc)
        case Block(_, expr) => occurring(expr, toTest, acc)
        case _ => acc
      }
    occurring(tree, boundVars(tree, Nil), Nil)
  }

  /** The instantiation direction for given poly param computed
   *  from the constraint:
   *  @return   1 (maximize) if constraint is uniformly from above,
   *           -1 (minimize) if constraint is uniformly from below,
   *            0 if unconstrained, or constraint is from below and above.
   */
  private def instDirection(param: TypeParamRef)(implicit ctx: Context): Int = {
    val constrained = ctx.typerState.constraint.fullBounds(param)
    val original = param.binder.paramInfos(param.paramNum)
    val cmp = ctx.typeComparer
    val approxBelow =
      if (!cmp.isSubTypeWhenFrozen(constrained.lo, original.lo)) 1 else 0
    val approxAbove =
      if (!cmp.isSubTypeWhenFrozen(original.hi, constrained.hi)) 1 else 0
    approxAbove - approxBelow
  }

  /** Following type aliases and stripping refinements and annotations, if one arrives at a
   *  class type reference where the class has a companion module, a reference to
   *  that companion module. Otherwise NoType
   */
  def companionRef(tp: Type)(implicit ctx: Context): Type =
    tp.underlyingClassRef(refinementOK = true) match {
      case tp: TypeRef =>
        val companion = tp.classSymbol.companionModule
        if (companion.exists)
          companion.termRef.asSeenFrom(tp.prefix, companion.owner)
        else NoType
      case _ => NoType
    }

  /** Instantiate undetermined type variables so that type `tp` is maximized.
   *  @return   The list of type symbols that were created
   *            to instantiate undetermined type variables that occur non-variantly
   */
  def maximizeType(tp: Type, span: Span, fromScala2x: Boolean)(implicit ctx: Context): List[Symbol] = Stats.track("maximizeType") {
    val vs = variances(tp)
    val patternBound = new mutable.ListBuffer[Symbol]
    vs foreachBinding { (tvar, v) =>
      if (v == 1) tvar.instantiate(fromBelow = false)
      else if (v == -1) tvar.instantiate(fromBelow = true)
      else {
        val bounds = ctx.typerState.constraint.fullBounds(tvar.origin)
        if (bounds.hi <:< bounds.lo || bounds.hi.classSymbol.is(Final) || fromScala2x)
          tvar.instantiate(fromBelow = false)
        else {
          val wildCard = ctx.newPatternBoundSymbol(UniqueName.fresh(tvar.origin.paramName), bounds, span)
          tvar.instantiateWith(wildCard.typeRef)
          patternBound += wildCard
        }
      }
    }
    patternBound.toList
  }

  type VarianceMap = SimpleIdentityMap[TypeVar, Integer]

  /** All occurrences of type vars in this type that satisfy predicate
   *  `include` mapped to their variances (-1/0/1) in this type, where
   *  -1 means: only covariant occurrences
   *  +1 means: only covariant occurrences
   *  0 means: mixed or non-variant occurrences
   *
   *  Note: We intentionally use a relaxed version of variance here,
   *  where the variance does not change under a prefix of a named type
   *  (the strict version makes prefixes invariant). This turns out to be
   *  better for type inference. In a nutshell, if a type variable occurs
   *  like this:
   *
   *     (U? >: x.type) # T
   *
   *  we want to instantiate U to x.type right away. No need to wait further.
   */
  private def variances(tp: Type)(implicit ctx: Context): VarianceMap = Stats.track("variances") {
    val constraint = ctx.typerState.constraint

    object accu extends TypeAccumulator[VarianceMap] {
      def setVariance(v: Int) = variance = v
      def apply(vmap: VarianceMap, t: Type): VarianceMap = t match {
        case t: TypeVar
        if !t.isInstantiated && ctx.typerState.constraint.contains(t) =>
          val v = vmap(t)
          if (v == null) vmap.updated(t, variance)
          else if (v == variance || v == 0) vmap
          else vmap.updated(t, 0)
        case _ =>
          foldOver(vmap, t)
      }
    }

    /** Include in `vmap` type variables occurring in the constraints of type variables
     *  already in `vmap`. Specifically:
     *   - if `tvar` is covariant in `vmap`, include all variables in its lower bound
     *     (because they influence the minimal solution of `tvar`),
     *   - if `tvar` is contravariant in `vmap`, include all variables in its upper bound
     *     at flipped variances (because they influence the maximal solution of `tvar`),
     *   - if `tvar` is nonvariant in `vmap`, include all variables in its upper and lower
     *     bounds as non-variant.
     *  Do this in a fixpoint iteration until `vmap` stabilizes.
     */
    def propagate(vmap: VarianceMap): VarianceMap = {
      var vmap1 = vmap
      def traverse(tp: Type) = { vmap1 = accu(vmap1, tp) }
      vmap.foreachBinding { (tvar, v) =>
        val param = tvar.origin
        val e = constraint.entry(param)
        accu.setVariance(v)
        if (v >= 0) {
          traverse(e.bounds.lo)
          constraint.lower(param).foreach(p => traverse(constraint.typeVarOfParam(p)))
        }
        if (v <= 0) {
          traverse(e.bounds.hi)
          constraint.upper(param).foreach(p => traverse(constraint.typeVarOfParam(p)))
        }
      }
      if (vmap1 eq vmap) vmap else propagate(vmap1)
    }

    propagate(accu(SimpleIdentityMap.Empty, tp))
  }
}

trait Inferencing { this: Typer =>
  import Inferencing._
  import tpd._

  /** Interpolate undetermined type variables in the widened type of this tree.
   *  @param tree    the tree whose type is interpolated
   *  @param pt      the expected result type
   *  @param locked  the set of type variables of the current typer state that cannot be interpolated
   *                 at the present time
   *  Eligible for interpolation are all type variables owned by the current typerstate
   *  that are not in locked. Type variables occurring co- (respectively, contra-) variantly in the type
   *  are minimized (respectvely, maximized). Non occurring type variables are minimized if they
   *  have a lower bound different from Nothing, maximized otherwise. Type variables appearing
   *  non-variantly in the type are left untouched.
   *
   *  Note that even type variables that do not appear directly in a type, can occur with
   *  some variance in the type, because of the constraints. E.g if `X` occurs co-variantly in `T`
   *  and we have a constraint
   *
   *      Y <: X
   *
   *  Then `Y` also occurs co-variantly in `T` because it needs to be minimized in order to constrain
   *  `T` the least. See `variances` for more detail.
   */
  def interpolateTypeVars(tree: Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): tree.type = {
    val state = ctx.typerState

    // Note that some variables in `locked` might not be in `state.ownedVars`
    // anymore if they've been garbage-collected, so we can't use
    // `state.ownedVars.size > locked.size` as an early check to avoid computing
    // `qualifying`.

    val ownedVars = state.ownedVars
    if ((ownedVars ne locked) && !ownedVars.isEmpty) {
      val qualifying = ownedVars -- locked
      if (!qualifying.isEmpty) {
        typr.println(i"interpolate $tree: ${tree.tpe.widen} in $state, owned vars = ${state.ownedVars.toList}%, %, previous = ${locked.toList}%, % / ${state.constraint}")
        val resultAlreadyConstrained =
          tree.isInstanceOf[Apply] || tree.tpe.isInstanceOf[MethodOrPoly]
        if (!resultAlreadyConstrained)
          constrainResult(tree.symbol, tree.tpe, pt)
            // This is needed because it could establish singleton type upper bounds. See i2998.scala.

        val tp = tree.tpe.widen
        val vs = variances(tp)

        // Avoid interpolating variables occurring in tree's type if typerstate has unreported errors.
        // Reason: The errors might reflect unsatisfiable constraints. In that
        // case interpolating without taking account the constraints risks producing
        // nonsensical types that then in turn produce incomprehensible errors.
        // An example is in neg/i1240.scala. Without the condition in the next code line
        // we get for
        //
        //      val y: List[List[String]] = List(List(1))
        //
        //     i1430.scala:5: error: type mismatch:
        //     found   : Int(1)
        //     required: Nothing
        //     val y: List[List[String]] = List(List(1))
        //                                           ^
        // With the condition, we get the much more sensical:
        //
        //     i1430.scala:5: error: type mismatch:
        //     found   : Int(1)
        //     required: String
        //     val y: List[List[String]] = List(List(1))
        val hasUnreportedErrors = state.reporter.hasUnreportedErrors
        def constraint = state.constraint
        for (tvar <- qualifying)
          if (!tvar.isInstantiated && state.constraint.contains(tvar)) {
            // Needs to be checked again, since previous interpolations could already have
            // instantiated `tvar` through unification.
            val v = vs(tvar)
            if (v == null) {
              typr.println(i"interpolate non-occurring $tvar in $state in $tree: $tp, fromBelow = ${tvar.hasLowerBound}, $constraint")
              tvar.instantiate(fromBelow = tvar.hasLowerBound)
            }
            else if (!hasUnreportedErrors)
              if (v.intValue != 0) {
                typr.println(i"interpolate $tvar in $state in $tree: $tp, fromBelow = ${v.intValue == 1}, $constraint")
                tvar.instantiate(fromBelow = v.intValue == 1)
              }
              else typr.println(i"no interpolation for nonvariant $tvar in $state")
          }
      }
    }
    tree
  }
}

/** An enumeration controlling the degree of forcing in "is-dully-defined" checks. */
@sharable object ForceDegree {
  class Value(val appliesTo: TypeVar => Boolean, val minimizeAll: Boolean, val allowBottom: Boolean = true)
  val none: Value = new Value(_ => false, minimizeAll = false)
  val all: Value = new Value(_ => true, minimizeAll = false)
  val noBottom: Value = new Value(_ => true, minimizeAll = false, allowBottom = false)
}

