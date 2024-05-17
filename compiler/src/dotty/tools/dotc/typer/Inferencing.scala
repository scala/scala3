package dotty.tools
package dotc
package typer

import core.*
import ast.*
import Contexts.*, Types.*, Flags.*, Symbols.*
import ProtoTypes.*
import NameKinds.UniqueName
import util.Spans.*
import util.{Stats, SimpleIdentityMap, SimpleIdentitySet, SrcPos}
import Decorators._
import config.Printers.{gadts, typr}
import annotation.tailrec
import reporting.*
import collection.mutable
import scala.annotation.internal.sharable

object Inferencing {

  import tpd.*

  /** Is type fully defined, meaning the type does not contain wildcard types
   *  or uninstantiated type variables. As a side effect, this will minimize
   *  any uninstantiated type variables, according to the given force degree,
   *  but only if the overall result of `isFullyDefined` is `true`.
   *  Variables that are successfully minimized do not count as uninstantiated.
   */
  def isFullyDefined(tp: Type, force: ForceDegree.Value)(using Context): Boolean =
    withFreshTyperState(new IsFullyDefinedAccumulator(force).process(tp), x => x)

  /** Try to fully define `tp`. Return whether constraint has changed.
   *  Any changed constraint is kept.
   */
  def canDefineFurther(tp: Type)(using Context): Boolean =
    val prevConstraint = ctx.typerState.constraint
    isFullyDefined(tp, force = ForceDegree.failBottom)
    && (ctx.typerState.constraint ne prevConstraint)

  /** The fully defined type, where all type variables are forced.
   *  Throws an error if type contains wildcards.
   */
  def fullyDefinedType(tp: Type, what: String, pos: SrcPos)(using Context): Type =
    try
      if isFullyDefined(tp, ForceDegree.all) then tp
      else throw new Error(i"internal error: type of $what $tp is not fully defined, pos = $pos")
    catch case ex: RecursionOverflow =>
      report.error(ex, pos)
      UnspecifiedErrorType

  /** Instantiate selected type variables `tvars` in type `tp` in a special mode:
   *   1. If a type variable is constrained from below (i.e. constraint bound != given lower bound)
   *      it is minimized.
   *   2. Otherwise, if the type variable is constrained from above, it is maximized.
   *   3. Otherwise, if the type variable has a lower bound != Nothing, it is minimized.
   *   4. Otherwise, if the type variable has an upper bound != Any, it is maximized.
   *  If none of (1) - (4) applies, the type variable is left uninstantiated.
   *  The method is called to instantiate type variables before an implicit search.
   */
  def instantiateSelected(tp: Type, tvars: List[Type])(using Context): Unit =
    if (tvars.nonEmpty)
      IsFullyDefinedAccumulator(
        new ForceDegree.Value(IfBottom.flip):
          override def appliesTo(tvar: TypeVar) = tvars.contains(tvar),
        minimizeSelected = true
      ).process(tp)

  /** Instantiate any type variables in `tp` whose bounds contain a reference to
   *  one of the parameters in `paramss`.
   */
  def instantiateDependent(tp: Type, paramss: List[List[Symbol]])(using Context): Unit = {
    val dependentVars = new TypeAccumulator[Set[TypeVar]] {
      def apply(tvars: Set[TypeVar], tp: Type) = tp match {
        case tp: TypeVar
        if !tp.isInstantiated &&
            TypeComparer.bounds(tp.origin)
              .namedPartsWith(ref => paramss.exists(_.contains(ref.symbol)))
              .nonEmpty =>
          tvars + tp
        case _ =>
          foldOver(tvars, tp)
      }
    }
    val depVars = dependentVars(Set(), tp)
    if (depVars.nonEmpty) instantiateSelected(tp, depVars.toList)
  }

  /** If `tp` is top-level type variable with a lower bound in the current constraint,
   *  instantiate it from below. We also look for TypeVars in other places where
   *  their instantiation could uncover new type members. However that search is best
   *  effort only. It might miss type variables that appear in structures involving
   *  alias types and type projections.
   *  @param applied  Test is done in a `tryInsertImplicitOnQualifier` application.
   *                  In this case, we always try to instantiate TypeVars in type arguments.
   *                  If `applied` is false, we only try that in arguments that may affect
   *                  the result type.
   */
  def couldInstantiateTypeVar(tp: Type, applied: Boolean = false)(using Context): Boolean = tp.dealias match
    case tvar: TypeVar
    if !tvar.isInstantiated
       && ctx.typerState.constraint.contains(tvar)
       && tvar.hasLowerBound =>
      tvar.instantiate(fromBelow = true)
      true
    case AppliedType(tycon, args) =>
      // The argument in `args` that may potentially appear directly as result
      // and thereby influence the members of this type
      def argsInResult: List[Type] = tycon.stripTypeVar match
        case tycon: TypeRef =>
          tycon.info match
            case MatchAlias(_) => args
            case TypeBounds(_, upper: TypeLambda) =>
              upper.resultType match
                case ref: TypeParamRef if ref.binder == upper =>
                  args.lazyZip(upper.paramRefs).collect {
                    case (arg, pref) if pref eq ref => arg
                  }.toList
                case _ => Nil
            case _ => Nil
        case _ => Nil
      couldInstantiateTypeVar(tycon, applied)
      || (if applied then args else argsInResult).exists(couldInstantiateTypeVar(_, applied))
    case RefinedType(parent, _, _) =>
      couldInstantiateTypeVar(parent, applied)
    case tp: AndOrType =>
      couldInstantiateTypeVar(tp.tp1, applied) || couldInstantiateTypeVar(tp.tp2, applied)
    case tp: AnnotatedType =>
      couldInstantiateTypeVar(tp.parent, applied)
    case _ =>
      false

  /** The accumulator which forces type variables using the policy encoded in `force`
   *  and returns whether the type is fully defined. The direction in which
   *  a type variable is instantiated is determined as follows:
   *   1. T is minimized if the constraint over T is only from below (i.e.
   *      constrained lower bound != given lower bound and
   *      constrained upper bound == given upper bound).
   *   2. T is maximized if the constraint over T is only from above (i.e.
   *      constrained upper bound != given upper bound and
   *      constrained lower bound == given lower bound).
   *
   *  If (1) and (2) do not apply, and minimizeSelected is set:
   *   3. T is minimized if it has a lower bound (different from Nothing) in the
   *      current constraint (the bound might come from T's declaration).
   *   4. Otherwise, T is maximized if it has an upper bound (different from Any)
   *      in the current constraint (the bound might come from T's declaration).
   *   5. Otherwise, T is not instantiated at all.

   *  If (1) and (2) do not apply, and minimizeSelected is not set:
   *   6: T is maximized if it appears only contravariantly in the given type,
   *      or if forceDegree is `flipBottom` and T has no lower bound different from Nothing.
   *   7. Otherwise, T is minimized.
   *
   *  The instantiation for (6) and (7) is done in two phases:
   *  1st Phase: Try to instantiate minimizable type variables to
   *  their lower bound. Record whether successful.
   *  2nd Phase: If first phase was successful, instantiate all remaining type variables
   *  to their upper bound.
   *
   *  Instance types can be improved by replacing covariant occurrences of Nothing
   *  with fresh type variables, if `force` allows this in its `canImprove` implementation.
   */
  private class IsFullyDefinedAccumulator(force: ForceDegree.Value, minimizeSelected: Boolean = false)
    (using Context) extends TypeAccumulator[Boolean] {

    /** Replace toplevel-covariant occurrences (i.e. covariant without double flips)
     *  of Nothing by fresh type variables. Double-flips are not covered to be
     *  conservative and save a bit of time on traversals; we could probably
     *  generalize that if we see use cases.
     *  For singleton types and references to module classes: try to
     *  improve the widened type. For module classes, the widened type
     *  is the intersection of all its non-transparent parent types.
     */
    private def improve(tvar: TypeVar) = new TypeMap:
      def apply(t: Type) = trace(i"improve $t", show = true):
        def tryWidened(widened: Type): Type =
          val improved = apply(widened)
          if improved ne widened then improved else mapOver(t)
        if variance > 0 then
          t match
            case t: TypeRef =>
              if t.symbol == defn.NothingClass then
                newTypeVar(TypeBounds.empty, nestingLevel = tvar.nestingLevel)
              else if t.symbol.is(ModuleClass) then
                tryWidened(t.parents.filter(!_.isTransparent())
                  .foldLeft(defn.AnyType: Type)(TypeComparer.andType(_, _)))
              else
                mapOver(t)
            case t: TermRef =>
              tryWidened(t.widen)
            case _ =>
              mapOver(t)
        else t

      // Don't map Nothing arguments for higher-kinded types; we'd get the wrong kind */
      override def mapArg(arg: Type, tparam: ParamInfo): Type =
        if tparam.paramInfo.isLambdaSub then arg
        else super.mapArg(arg, tparam)
    end improve

    /** Instantiate type variable with possibly improved computed instance type.
     *  @return  true if variable was instantiated with improved type, which
     *           in this case should not be instantiated further, false otherwise.
     */
    private def instantiate(tvar: TypeVar, fromBelow: Boolean): Boolean =
      if fromBelow && force.canImprove(tvar) then
        val inst = tvar.typeToInstantiateWith(fromBelow = true)
        if isFullyDefined(inst, ForceDegree.none) then
          // need to recursively check before improving, since improving adds type vars
          // which should not be instantiated at this point
          val better = improve(tvar)(inst)
          if better <:< TypeComparer.fullUpperBound(tvar.origin) then
            typr.println(i"forced instantiation of invariant ${tvar.origin} = $inst, improved to $better")
            tvar.instantiateWith(better)
            return true
      val inst = tvar.instantiate(fromBelow)
      typr.println(i"forced instantiation of ${tvar.origin} = $inst")
      false

    private var toMaximize: List[TypeVar] = Nil

    def apply(x: Boolean, tp: Type): Boolean = trace(i"isFullyDefined($tp, $force)", typr) {
      try {
      val tpd = tp.dealias
      if tpd ne tp then apply(x, tpd)
      else tp match
        case _: WildcardType | _: ProtoType =>
          false
        case tvar: TypeVar if !tvar.isInstantiated =>
          force.appliesTo(tvar)
          && ctx.typerState.constraint.contains(tvar)
          && {
            var fail = false
            var skip = false
            val direction = instDirection(tvar.origin)
            if minimizeSelected then
              if direction <= 0 && tvar.hasLowerBound then
                skip = instantiate(tvar, fromBelow = true)
              else if direction >= 0 && tvar.hasUpperBound then
                skip = instantiate(tvar, fromBelow = false)
              // else hold off instantiating unbounded unconstrained variable
            else if direction != 0 then
              skip = instantiate(tvar, fromBelow = direction < 0)
            else if variance >= 0 && tvar.hasLowerBound then
              skip = instantiate(tvar, fromBelow = true)
            else if (variance > 0 || variance == 0 && !tvar.hasUpperBound)
                && force.ifBottom == IfBottom.ok
            then // if variance == 0, prefer upper bound if one is given
              skip = instantiate(tvar, fromBelow = true)
            else if variance >= 0 && force.ifBottom == IfBottom.fail then
              fail = true
            else
              toMaximize = tvar :: toMaximize
            !fail && (skip || foldOver(x, tvar))
          }
        case tp => foldOver(x, tp)
      }
      catch case ex: Throwable =>
        handleRecursive("check fully defined", tp.showSummary(20), ex)
    }

    def process(tp: Type): Boolean =
      // Maximize type vars in the order they were visited before */
      def maximize(tvars: List[TypeVar]): Unit = tvars match
        case tvar :: tvars1 =>
          maximize(tvars1)
          if !tvar.isInstantiated then
            instantiate(tvar, fromBelow = false)
        case nil =>
      apply(true, tp)
      && (
        toMaximize.isEmpty
        || { maximize(toMaximize)
             toMaximize = Nil       // Do another round since the maximixing instances
             process(tp)            // might have type uninstantiated variables themselves.
           }
      )
  }

  def approximateGADT(tp: Type)(using Context): Type = {
    val map = new ApproximateGadtAccumulator
    val res = map(tp)
    assert(!map.failed)
    res
  }

  /** Approximates a type to get rid of as many GADT-constrained abstract types as possible. */
  private class ApproximateGadtAccumulator(using Context) extends TypeMap {

    var failed = false

    /** GADT approximation proceeds differently from type variable approximation.
      *
      * Essentially, what we're doing is we're inferring a type ascription that
      * will remove as many GADT-constrained types as possible. This means that
      * we want to approximate type T to type S in such a way that no matter how
      * GADT-constrained types are instantiated, T <: S. In other words, the
      * relationship _necessarily_ must hold.
      *
      * We accomplish that by:
      *   - replacing covariant occurrences with upper GADT bound
      *   - replacing contravariant occurrences with lower GADT bound
      *   - leaving invariant occurrences alone
      *
      * Examples:
      *   - If we have GADT cstr A <: Int, then for all A <: Int, Option[A] <: Option[Int].
      *     Therefore, we can approximate Option[A] ~~ Option[Int].
      *   - If we have A >: S <: T, then for all such A, A => A <: S => T. This
      *     illustrates that it's fine to differently approximate different
      *     occurrences of same type.
      *   - If we have A <: Int and F <: [A] => Option[A] (note the invariance),
      *     then we should approximate F[A] ~~ Option[A]. That is, we should
      *     respect the invariance of the type constructor.
      *   - If we have A <: Option[B] and B <: Int, we approximate A ~~
      *     Option[B]. That is, we don't recurse into already approximated
      *     types. Since GADT approximation is (for now) only used for member
      *     selection, this behaviour is expected, as nested types cannot affect
      *     member selection (note that given/extension lookup doesn't need GADT
      *     approx, see gadt-approximation-interaction.scala).
      */
    def apply(tp: Type): Type = tp.dealias match {
      case tp @ TypeRef(qual, nme) if variance != 0
                                   && ctx.gadt.contains(tp.symbol)
                                   =>
        val sym = tp.symbol
        val res = ctx.gadtState.approximation(sym, fromBelow = variance < 0)
        gadts.println(i"approximated $tp  ~~  $res")
        res

      case _: WildcardType | _: ProtoType =>
        failed = true
        NoType

      case tp =>
        mapOver(tp)
    }

    def process(tp: Type): Type = {
      apply(tp)
    }
  }

  /** For all type parameters occurring in `tp`:
   *  If the bounds of `tp` in the current constraint are equal wrt =:=,
   *  instantiate the type parameter to the lower bound's approximation
   *  (approximation because of possible F-bounds).
   */
  def replaceSingletons(tp: Type)(using Context): Unit = {
    val tr = new TypeTraverser {
      def traverse(tp: Type): Unit = {
        tp match {
          case param: TypeParamRef =>
            val constraint = accCtx.typerState.constraint
            constraint.entry(param) match {
              case TypeBounds(lo, hi)
              if (hi frozen_<:< lo) =>
                val inst = TypeComparer.approximation(param, fromBelow = true)
                typr.println(i"replace singleton $param := $inst")
                accCtx.typerState.constraint = constraint.replace(param, inst)
              case _ =>
            }
          case _ =>
        }
        traverseChildren(tp)
      }
    }
    tr.traverse(tp)
  }

  /** If `tree` has a type lambda type, infer its type parameters by comparing with expected type `pt` */
  def inferTypeParams(tree: Tree, pt: Type)(using Context): Tree = tree.tpe match
    case tl: TypeLambda =>
      val (tl1, tvars) = constrained(tl, tree)
      val tree1 = AppliedTypeTree(tree.withType(tl1), tvars.map(_.wrapInTypeTree(tree)))
      tree1.tpe <:< pt
      if isFullyDefined(tree1.tpe, force = ForceDegree.failBottom) then
        tree1
      else
        EmptyTree
    case _ =>
      tree

  def isSkolemFree(tp: Type)(using Context): Boolean =
    !tp.existsPart(_.isInstanceOf[SkolemType])

  /** The list of uninstantiated type variables bound by some prefix of type `T` or
   *  by arguments of an application prefix, which occur at least once as a formal type parameter
   *  of an application either from a prefix or an argument of an application node.
   *  Considered prefixes are:
   *    - The function `f` of an application node `f(e1, .., en)`
   *    - The function `f` of a type application node `f[T1, ..., Tn]`
   *    - The prefix `p` of a selection `p.f`.
   *    - The result expression `e` of a block `{s1; .. sn; e}`.
   */
  def tvarsInParams(tree: Tree, locked: TypeVars)(using Context): List[TypeVar] = {
    def boundVars(tree: Tree, acc: List[TypeVar]): List[TypeVar] = tree match {
      case Apply(fn, args) =>
        val argTpVars = args.flatMap(boundVars(_, Nil))
        boundVars(fn, acc ++ argTpVars)
      case TypeApply(fn, targs) =>
        val tvars = targs.filter(_.isInstanceOf[InferredTypeTree]).tpes.collect {
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
    def occurring(tree: Tree, toTest: List[TypeVar], acc: List[TypeVar]): List[TypeVar] =
      if (toTest.isEmpty) acc
      else tree match {
        case Apply(fn, args) =>
          val argsOcc = args.flatMap(occurring(_, toTest, Nil))
          val argsNocc = toTest.filterNot(argsOcc.contains)
          fn.tpe.widen match {
            case mtp: MethodType =>
              val (occ, nocc) = argsNocc.partition(tvar => mtp.paramInfos.exists(tvar.occursIn))
              occurring(fn, nocc, occ ::: argsOcc ::: acc)
            case _ =>
              occurring(fn, argsNocc, argsOcc ::: acc)
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
  private def instDirection(param: TypeParamRef)(using Context): Int = {
    val constrained = TypeComparer.fullBounds(param)
    val original = param.binder.paramInfos(param.paramNum)
    val cmp = TypeComparer
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
  def companionRef(tp: Type)(using Context): Type =
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
  def maximizeType(tp: Type, span: Span)(using Context): List[Symbol] = {
    Stats.record("maximizeType")
    val vs = variances(tp)
    val patternBindings = new mutable.ListBuffer[(Symbol, TypeParamRef)]
    val gadtBounds = ctx.gadt.symbols.map(ctx.gadt.bounds(_).nn)
    vs.underlying foreachBinding { (tvar, v) =>
      if !tvar.isInstantiated then
        // if the tvar is covariant/contravariant (v == 1/-1, respectively) in the input type tp
        // then it is safe to instantiate if it doesn't occur in any of the GADT bounds.
        // Eg neg/i14983 the C in Node[+C] occurs in GADT bound X >: List[C] so maximising to Node[Any] is unsound
        // Eg pos/precise-pattern-type the T in Tree[-T] doesn't occur in any GADT bound so can maximise to Tree[Type]
        val safeToInstantiate = v != 0 && gadtBounds.forall(!tvar.occursIn(_))
        if safeToInstantiate then tvar.instantiate(fromBelow = v == -1)
        else {
          val bounds = TypeComparer.fullBounds(tvar.origin)
          if (bounds.hi frozen_<:< bounds.lo) || bounds.hi.classSymbol.is(Final) then
            tvar.instantiate(fromBelow = false)
          else {
            // We do not add the created symbols to GADT constraint immediately, since they may have inter-dependencies.
            // Instead, we simultaneously add them later on.
            val wildCard = newPatternBoundSymbol(UniqueName.fresh(tvar.origin.paramName), bounds, span, addToGadt = false)
            tvar.instantiateWith(wildCard.typeRef)
            patternBindings += ((wildCard, tvar.origin))
          }
        }
    }
    val res = patternBindings.toList.map { (boundSym, _) =>
      // substitute bounds of pattern bound variables to deal with possible F-bounds
      for (wildCard, param) <- patternBindings do
        boundSym.info = boundSym.info.substParam(param, wildCard.typeRef)
      boundSym
    }

    // We add the created symbols to GADT constraint here.
    if (res.nonEmpty) ctx.gadtState.addToConstraint(res)
    res
  }

  /** All occurrences of type vars in `tp` that satisfy predicate
   *  `include` mapped to their variances (-1/0/1) in both `tp` and
   *  `pt.finalResultType`, where
   *  -1 means: only contravariant occurrences
   *  +1 means: only covariant occurrences
   *  0 means: mixed or non-variant occurrences
   *
   *  We need to take the occurrences in `pt` into account because a type
   *  variable created when typing the current tree might only appear in the
   *  bounds of a type variable in the expected type, for example when
   *  `ConstraintHandling#legalBound` creates type variables when approximating
   *  a bound.
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
  def variances(tp: Type, pt: Type = WildcardType)(using Context): VarianceMap[TypeVar] = {
    Stats.record("variances")
    val constraint = ctx.typerState.constraint

    object accu extends TypeAccumulator[VarianceMap[TypeVar]]:
      def setVariance(v: Int) = variance = v
      def apply(vmap: VarianceMap[TypeVar], t: Type): VarianceMap[TypeVar] = t match
        case t: TypeVar
        if !t.isInstantiated && accCtx.typerState.constraint.contains(t) =>
          vmap.recordLocalVariance(t, variance)
        case _ =>
          foldOver(vmap, t)

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
    def propagate(vmap: VarianceMap[TypeVar]): VarianceMap[TypeVar] = {
      var vmap1 = vmap
      def traverse(tp: Type) = { vmap1 = accu(vmap1, tp) }
      vmap.underlying.foreachBinding { (tvar, v) =>
        val param = tvar.origin
        constraint.entry(param) match
          case TypeBounds(lo, hi) =>
            accu.setVariance(v)
            if v >= 0 then
              traverse(lo)
              constraint.lower(param).foreach(p => traverse(constraint.typeVarOfParam(p)))
            if v <= 0 then
              traverse(hi)
              constraint.upper(param).foreach(p => traverse(constraint.typeVarOfParam(p)))
          case _ =>
      }
      if (vmap1 eq vmap) vmap else propagate(vmap1)
    }

    propagate(accu(accu(VarianceMap.empty, tp), pt.finalResultType))
  }

  /** Run the transformation after dealiasing but return the original type if it was a no-op. */
  private def derivedOnDealias(tp: Type)(transform: Type => Type)(using Context) = {
    val dealiased = tp.dealias
    val transformed = transform(dealiased)
    if transformed eq dealiased then tp // return the original type, not the result of dealiasing
    else transformed
  }

  /** Replace every top-level occurrence of a wildcard type argument by
   *  a fresh skolem type. The skolem types are of the form $i.CAP, where
   *  $i is a skolem of type `scala.internal.TypeBox`, and `CAP` is its
   *  type member. See the documentation of `TypeBox` for a rationale why we do this.
   */
  def captureWildcards(tp: Type)(using Context): Type = derivedOnDealias(tp) {
    case tp @ AppliedType(tycon, args) if tp.hasWildcardArg =>
      val tparams = tycon.typeParamSymbols
      val args1 = args.zipWithConserve(tparams.map(_.paramInfo.substApprox(tparams, args))) {
        case (TypeBounds(lo, hi), bounds) =>
          val skolem = SkolemType(defn.TypeBoxClass.typeRef.appliedTo(lo | bounds.loBound, hi & bounds.hiBound))
          TypeRef(skolem, defn.TypeBox_CAP)
        case (arg, _) =>
          arg
      }
      if tparams.isEmpty then tp else tp.derivedAppliedType(tycon, args1)
    case tp: AndOrType => tp.derivedAndOrType(captureWildcards(tp.tp1), captureWildcards(tp.tp2))
    case tp: RefinedType => tp.derivedRefinedType(parent = captureWildcards(tp.parent))
    case tp: RecType => tp.derivedRecType(captureWildcards(tp.parent))
    case tp: LazyRef => captureWildcards(tp.ref)
    case tp: AnnotatedType => tp.derivedAnnotatedType(captureWildcards(tp.parent), tp.annot)
    case _ => tp
  }

  def hasCaptureConversionArg(tp: Type)(using Context): Boolean = tp match
    case tp: AppliedType => tp.args.exists(_.typeSymbol == defn.TypeBox_CAP)
    case _ => false
}

trait Inferencing { this: Typer =>
  import Inferencing.*
  import tpd.*

  /** Interpolate undetermined type variables in the widened type of this tree.
   *  @param tree    the tree whose type is interpolated
   *  @param pt      the expected result type
   *  @param locked  the set of type variables of the current typer state that cannot be interpolated
   *                 at the present time
   *  Eligible for interpolation are all type variables owned by the current typerstate
   *  that are not in `locked` and whose `nestingLevel` is `>= ctx.nestingLevel`.
   *  Type variables occurring co- (respectively, contra-) variantly in the tree type
   *  or expected type are minimized (respectively, maximized). Non occurring type variables are minimized if they
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
  def interpolateTypeVars(tree: Tree, pt: Type, locked: TypeVars)(using Context): tree.type =
    val state = ctx.typerState

    // Note that some variables in `locked` might not be in `state.ownedVars`
    // anymore if they've been garbage-collected, so we can't use
    // `state.ownedVars.size > locked.size` as an early check to avoid computing
    // `qualifying`.

    val ownedVars = state.ownedVars
    if (ownedVars ne locked) && !ownedVars.isEmpty then
      val qualifying = ownedVars -- locked
      if (!qualifying.isEmpty) {
        typr.println(i"interpolate $tree: ${tree.tpe.widen} in $state, pt = $pt, owned vars = ${state.ownedVars.toList}%, %, qualifying = ${qualifying.toList}%, %, previous = ${locked.toList}%, % / ${state.constraint}")
        val resultAlreadyConstrained =
          tree.isInstanceOf[Apply] || tree.tpe.isInstanceOf[MethodOrPoly]
        if (!resultAlreadyConstrained)
          constrainResult(tree.symbol, tree.tpe, pt)
            // This is needed because it could establish singleton type upper bounds. See i2998.scala.

        val tp = tree.tpe.widen
        val vs = variances(tp, pt)

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
        if state.reporter.hasUnreportedErrors then return tree

        def constraint = state.constraint

        /** Values of this type report type variables to instantiate with variance indication:
         *    +1  variable appears covariantly, can be instantiated from lower bound
         *    -1  variable appears contravariantly, can be instantiated from upper bound
         *     0  variable does not appear at all, can be instantiated from either bound
         */
        type ToInstantiate = List[(TypeVar, Int)]

        val toInstantiate: ToInstantiate =
          val buf = new mutable.ListBuffer[(TypeVar, Int)]
          for tvar <- qualifying do
            if !tvar.isInstantiated && constraint.contains(tvar) && tvar.nestingLevel >= ctx.nestingLevel then
              constrainIfDependentParamRef(tvar, tree)
              if !tvar.isInstantiated then
                // isInstantiated needs to be checked again, since previous interpolations could already have
                // instantiated `tvar` through unification.
                val v = vs.computedVariance(tvar)
                if v == null then buf += ((tvar, 0))
                else if v.intValue != 0 then buf += ((tvar, v.intValue))
                else comparing(cmp =>
                  if !cmp.levelOK(tvar.nestingLevel, ctx.nestingLevel) then
                    // Invariant: The type of a tree whose enclosing scope is level
                    // N only contains type variables of level <= N.
                    typr.println(i"instantiate nonvariant $tvar of level ${tvar.nestingLevel} to a type variable of level <= ${ctx.nestingLevel}, $constraint")
                    cmp.atLevel(ctx.nestingLevel, tvar.origin)
                  else
                    typr.println(i"no interpolation for nonvariant $tvar in $state")
                )
          // constrainIfDependentParamRef could also have instantiated tvars added to buf before the check
          buf.filterNot(_._1.isInstantiated).toList
        end toInstantiate

        def typeVarsIn(xs: ToInstantiate): TypeVars =
          xs.foldLeft(SimpleIdentitySet.empty: TypeVars)((tvs, tvi) => tvs + tvi._1)

        /** Filter list of proposed instantiations so that they don't constrain further
         *  the current constraint.
         */
        def filterByDeps(tvs0: ToInstantiate): ToInstantiate =
          val excluded =  // ignore dependencies from other variables that are being instantiated
            typeVarsIn(tvs0)
          def step(tvs: ToInstantiate): ToInstantiate = tvs match
            case tvs @ (hd @ (tvar, v)) :: tvs1 =>
              def aboveOK = !constraint.dependsOn(tvar, excluded, co = true)
              def belowOK = !constraint.dependsOn(tvar, excluded, co = false)
              if v == 0 && !aboveOK then
                step((tvar, 1) :: tvs1)
              else if v == 0 && !belowOK then
                step((tvar, -1) :: tvs1)
              else if v == -1 && !aboveOK || v == 1 && !belowOK then
                typr.println(i"drop $tvar, $v in $tp, $pt, qualifying = ${qualifying.toList}, tvs0 = ${tvs0.toList}%, %, excluded = ${excluded.toList}, $constraint")
                step(tvs1)
              else // no conflict, keep the instantiation proposal
                tvs.derivedCons(hd, step(tvs1))
            case Nil =>
              Nil
          val tvs1 = step(tvs0)
          if tvs1 eq tvs0 then tvs1
          else filterByDeps(tvs1) // filter again with smaller excluded set
        end filterByDeps

        /** Instantiate all type variables in `tvs` in the indicated directions,
         *  as described in the doc comment of `ToInstantiate`.
         *  If a type variable A is instantiated from below, and there is another
         *  type variable B in `buf` that is known to be smaller than A, wait and
         *  instantiate all other type variables before trying to instantiate A again.
         *  Dually, wait instantiating a type variable from above as long as it has
         *  upper bounds in `buf`.
         *
         *  This is done to avoid loss of precision when forming unions. An example
         *  is in i7558.scala:
         *
         *      type Tr[+V1, +O1 <: V1]
         *      extension [V2, O2 <: V2](tr: Tr[V2, O2]) def sl: Tr[V2, O2] = ???
         *      def as[V3, O3 <: V3](tr: Tr[V3, O3]) : Tr[V3, O3] = tr.sl
         *
         *   Here we interpolate at some point V2 and O2 given the constraint
         *
         *      V2 >: V3, O2 >: O3, O2 <: V2
         *
         *   where O3 and V3 are type refs with O3 <: V3.
         *   If we interpolate V2 first to V3 | O2, the widenUnion algorithm will
         *   instantiate O2 to V3, leading to the final constraint
         *
         *      V2 := V3, O2 := V3
         *
         *   But if we instantiate O2 first to O3, and V2 next to V3, we get the
         *   more flexible instantiation
         *
         *      V2 := V3, O2 := O3
         */
        def doInstantiate(tvs: ToInstantiate): Unit =

          /** Try to instantiate `tvs`, return any suspended type variables */
          def tryInstantiate(tvs: ToInstantiate): ToInstantiate = tvs match
            case (hd @ (tvar, v)) :: tvs1 =>
              val fromBelow = v == 1 || (v == 0 && tvar.hasLowerBound)
              typr.println(
                i"interpolate${if v == 0 then " non-occurring" else ""} $tvar in $state in $tree: $tp, fromBelow = $fromBelow, $constraint")
              if tvar.isInstantiated then
                tryInstantiate(tvs1)
              else
                val suspend = tvs1.exists{ (following, _) =>
                  if fromBelow
                  then constraint.isLess(following.origin, tvar.origin)
                  else constraint.isLess(tvar.origin, following.origin)
                }
                if suspend then
                  typr.println(i"suspended: $hd")
                  hd :: tryInstantiate(tvs1)
                else
                  tvar.instantiate(fromBelow)
                  tryInstantiate(tvs1)
            case Nil => Nil
          if tvs.nonEmpty then doInstantiate(tryInstantiate(tvs))
        end doInstantiate

        doInstantiate(filterByDeps(toInstantiate))
      }
    end if
    tree
  end interpolateTypeVars

  /** If `tvar` represents a parameter of a dependent method type in the current `call`
   *  approximate it from below with the type of the actual argument. Skolemize that
   *  type if necessary to make it a Singleton.
   */
  private def constrainIfDependentParamRef(tvar: TypeVar, call: Tree)(using Context): Unit =
    if tvar.origin.paramName.is(NameKinds.DepParamName) then
      representedParamRef(tvar.origin) match
        case ref: TermParamRef =>
          def findArg(tree: Tree)(using Context): Tree = tree match
            case Apply(fn, args) =>
              if fn.tpe.widen eq ref.binder then
                if ref.paramNum < args.length then args(ref.paramNum)
                else EmptyTree
              else findArg(fn)
            case TypeApply(fn, _) => findArg(fn)
            case Block(_, expr) => findArg(expr)
            case Inlined(_, _, expr) => findArg(expr)
            case _ => EmptyTree

          val arg = findArg(call)
          if !arg.isEmpty then
            var argType = arg.tpe.widenIfUnstable
            if !argType.isSingleton then argType = SkolemType(argType)
            argType <:< tvar
        case _ =>
  end constrainIfDependentParamRef
}

/** An enumeration controlling the degree of forcing in "is-fully-defined" checks. */
@sharable object ForceDegree:
  class Value(val ifBottom: IfBottom):

    /** Does `tv` need to be instantiated? */
    def appliesTo(tv: TypeVar): Boolean = true

    /** Should we try to improve the computed instance type by replacing bottom types
     *  with fresh type variables?
     */
    def canImprove(tv: TypeVar): Boolean = false

    override def toString = s"ForceDegree.Value($ifBottom)"
  end Value

  val none: Value = new Value(IfBottom.ok):
    override def appliesTo(tv: TypeVar) = false
    override def toString = "ForceDegree.none"
  val all: Value = new Value(IfBottom.ok):
    override def toString = "ForceDegree.all"
  val failBottom: Value = new Value(IfBottom.fail):
    override def toString = "ForceDegree.failBottom"
  val flipBottom: Value = new Value(IfBottom.flip):
    override def toString = "ForceDegree.flipBottom"
end ForceDegree

enum IfBottom:
  case ok, fail, flip
