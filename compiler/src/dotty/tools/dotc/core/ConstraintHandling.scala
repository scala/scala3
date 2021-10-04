package dotty.tools
package dotc
package core

import Types._
import Contexts._
import Symbols._
import Decorators._
import Flags._
import config.Config
import config.Printers.typr
import reporting.trace
import typer.ProtoTypes.newTypeVar
import StdNames.tpnme

/** Methods for adding constraints and solving them.
 *
 * What goes into a Constraint as opposed to a ConstrainHandler?
 *
 * Constraint code is purely functional: Operations get constraints and produce new ones.
 * Constraint code does not have access to a type-comparer. Anything regarding lubs and glbs has to be done
 * elsewhere.
 *
 * By comparison: Constraint handlers are parts of type comparers and can use their functionality.
 * Constraint handlers update the current constraint as a side effect.
 */
trait ConstraintHandling {

  def constr: config.Printers.Printer = config.Printers.constr

  protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean
  protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean

  protected def constraint: Constraint
  protected def constraint_=(c: Constraint): Unit

  private var addConstraintInvocations = 0

  /** If the constraint is frozen we cannot add new bounds to the constraint. */
  protected var frozenConstraint: Boolean = false

  /** Potentially a type lambda that is still instantiatable, even though the constraint
   *  is generally frozen.
   */
  protected var caseLambda: Type = NoType

  /** If set, align arguments `S1`, `S2`when taking the glb
   *  `T1 { X = S1 } & T2 { X = S2 }` of a constraint upper bound for some type parameter.
   *  Aligning means computing `S1 =:= S2` which may change the current constraint.
   *  See note in TypeComparer#distributeAnd.
   */
  protected var homogenizeArgs: Boolean = false

  /** We are currently comparing type lambdas. Used as a flag for
   *  optimization: when `false`, no need to do an expensive `pruneLambdaParams`
   */
  protected var comparedTypeLambdas: Set[TypeLambda] = Set.empty

  def checkReset() =
    assert(addConstraintInvocations == 0)
    assert(frozenConstraint == false)
    assert(caseLambda == NoType)
    assert(homogenizeArgs == false)
    assert(comparedTypeLambdas == Set.empty)

  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds = constraint.nonParamBounds(param)

  def fullLowerBound(param: TypeParamRef)(using Context): Type =
    constraint.minLower(param).foldLeft(nonParamBounds(param).lo)(_ | _)

  def fullUpperBound(param: TypeParamRef)(using Context): Type =
    constraint.minUpper(param).foldLeft(nonParamBounds(param).hi)(_ & _)

  /** Full bounds of `param`, including other lower/upper params.
    *
    * Note that underlying operations perform subtype checks - for this reason, recursing on `fullBounds`
    * of some param when comparing types might lead to infinite recursion. Consider `bounds` instead.
    */
  def fullBounds(param: TypeParamRef)(using Context): TypeBounds =
    nonParamBounds(param).derivedTypeBounds(fullLowerBound(param), fullUpperBound(param))

  /** If true, eliminate wildcards in bounds by avoidance, otherwise replace
   *  them by fresh variables.
   */
  protected def approximateWildcards: Boolean = true

  protected def addOneBound(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Boolean =
    if !constraint.contains(param) then true
    else if !isUpper && param.occursIn(rawBound) then
      // We don't allow recursive lower bounds when defining a type,
      // so we shouldn't allow them as constraints either.
      false
    else
      val dropWildcards = new AvoidWildcardsMap:
        if !isUpper then variance = -1
        override def mapWild(t: WildcardType) =
          if approximateWildcards then super.mapWild(t)
          else newTypeVar(apply(t.effectiveBounds).toBounds)
      val bound = dropWildcards(rawBound)
      val oldBounds @ TypeBounds(lo, hi) = constraint.nonParamBounds(param)
      val equalBounds = (if isUpper then lo else hi) eq bound
      if equalBounds && !bound.existsPart(_ eq param, StopAt.Static) then
        // The narrowed bounds are equal and not recursive,
        // so we can remove `param` from the constraint.
        constraint = constraint.replace(param, bound)
        true
      else
        // Narrow one of the bounds of type parameter `param`
        // If `isUpper` is true, ensure that `param <: `bound`, otherwise ensure
        // that `param >: bound`.
        val narrowedBounds =
          val saved = homogenizeArgs
          homogenizeArgs = Config.alignArgsInAnd
          try
            if isUpper then oldBounds.derivedTypeBounds(lo, hi & bound)
            else oldBounds.derivedTypeBounds(lo | bound, hi)
          finally homogenizeArgs = saved
        val c1 = constraint.updateEntry(param, narrowedBounds)
        (c1 eq constraint)
        || {
          constraint = c1
          val TypeBounds(lo, hi) = constraint.entry(param)
          isSub(lo, hi)
        }
  end addOneBound

  protected def addBoundTransitively(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Boolean =

    /** Adjust the bound `tp` in the following ways:
     *
     *   1. Toplevel occurrences of TypeRefs that are instantiated in the current
     *      constraint are also dereferenced.
     *   2. Toplevel occurrences of ExprTypes lead to a `NoType` return, which
     *      causes the addOneBound operation to fail.
     *
     *   An occurrence is toplevel if it is the bound itself, or a term in some
     *   combination of `&` or `|` types.
     */
    def adjust(tp: Type): Type = tp match
      case tp: AndOrType =>
        val p1 = adjust(tp.tp1)
        val p2 = adjust(tp.tp2)
        if p1.exists && p2.exists then tp.derivedAndOrType(p1, p2) else NoType
      case tp: TypeVar if constraint.contains(tp.origin) =>
        adjust(tp.underlying)
      case tp: ExprType =>
        // ExprTypes are not value types, so type parameters should not
        // be instantiated to ExprTypes. A scenario where such an attempted
        // instantiation can happen is if we unify (=> T) => () with A => ()
        // where A is a TypeParamRef. See the comment on EtaExpansion.etaExpand
        // why types such as (=> T) => () can be constructed and i7969.scala
        // as a test where this happens.
        // Note that scalac by contrast allows such instantiations. But letting
        // type variables be ExprTypes has its own problems (e.g. you can't write
        // the resulting types down) and is largely unknown terrain.
        NoType
      case _ =>
        tp

    def description = i"constraint $param ${if isUpper then "<:" else ":>"} $rawBound to\n$constraint"
    constr.println(i"adding $description$location")
    if isUpper && rawBound.isRef(defn.NothingClass) && ctx.typerState.isGlobalCommittable then
      def msg = i"!!! instantiated to Nothing: $param, constraint = $constraint"
      if Config.failOnInstantiationToNothing
      then assert(false, msg)
      else report.log(msg)
    def others = if isUpper then constraint.lower(param) else constraint.upper(param)
    val bound = adjust(rawBound)
    bound.exists
    && addOneBound(param, bound, isUpper) && others.forall(addOneBound(_, bound, isUpper))
        .showing(i"added $description = $result$location", constr)
  end addBoundTransitively

  protected def addLess(p1: TypeParamRef, p2: TypeParamRef)(using Context): Boolean = {
    def description = i"ordering $p1 <: $p2 to\n$constraint"
    val res =
      if (constraint.isLess(p2, p1)) unify(p2, p1)
      else {
        val down1 = p1 :: constraint.exclusiveLower(p1, p2)
        val up2 = p2 :: constraint.exclusiveUpper(p2, p1)
        val lo1 = constraint.nonParamBounds(p1).lo
        val hi2 = constraint.nonParamBounds(p2).hi
        constr.println(i"adding $description down1 = $down1, up2 = $up2$location")
        constraint = constraint.addLess(p1, p2)
        down1.forall(addOneBound(_, hi2, isUpper = true)) &&
        up2.forall(addOneBound(_, lo1, isUpper = false))
      }
    constr.println(i"added $description = $res$location")
    res
  }

  def location(using Context) = "" // i"in ${ctx.typerState.stateChainStr}" // use for debugging

  /** Make p2 = p1, transfer all bounds of p2 to p1
   *  @pre  less(p1)(p2)
   */
  private def unify(p1: TypeParamRef, p2: TypeParamRef)(using Context): Boolean = {
    constr.println(s"unifying $p1 $p2")
    assert(constraint.isLess(p1, p2))
    constraint = constraint.addLess(p2, p1)
    val down = constraint.exclusiveLower(p2, p1)
    val up = constraint.exclusiveUpper(p1, p2)
    constraint = constraint.unify(p1, p2)
    val bounds = constraint.nonParamBounds(p1)
    val lo = bounds.lo
    val hi = bounds.hi
    isSub(lo, hi) &&
    down.forall(addOneBound(_, hi, isUpper = true)) &&
    up.forall(addOneBound(_, lo, isUpper = false))
  }

  protected def isSubType(tp1: Type, tp2: Type, whenFrozen: Boolean)(using Context): Boolean =
    if (whenFrozen)
      isSubTypeWhenFrozen(tp1, tp2)
    else
      isSub(tp1, tp2)

  inline final def inFrozenConstraint[T](op: => T): T = {
    val savedFrozen = frozenConstraint
    val savedLambda = caseLambda
    frozenConstraint = true
    caseLambda = NoType
    try op
    finally {
      frozenConstraint = savedFrozen
      caseLambda = savedLambda
    }
  }

  final def isSubTypeWhenFrozen(tp1: Type, tp2: Type)(using Context): Boolean = inFrozenConstraint(isSub(tp1, tp2))
  final def isSameTypeWhenFrozen(tp1: Type, tp2: Type)(using Context): Boolean = inFrozenConstraint(isSame(tp1, tp2))

  /** Test whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   */
  protected final def isSatisfiable(using Context): Boolean =
    constraint.forallParams { param =>
      val TypeBounds(lo, hi) = constraint.entry(param)
      isSub(lo, hi) || {
        report.log(i"sub fail $lo <:< $hi")
        false
      }
    }

  /** Solve constraint set for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound, unless the upper bound
   *  contains a reference to the parameter itself (such occurrences can arise
   *  for F-bounded types, `addOneBound` ensures that they never occur in the
   *  lower bound).
   *  Wildcard types in bounds are approximated by their upper or lower bounds.
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  final def approximation(param: TypeParamRef, fromBelow: Boolean)(using Context): Type =
    constraint.entry(param) match
      case entry: TypeBounds =>
        val useLowerBound = fromBelow || param.occursIn(entry.hi)
        val inst = if useLowerBound then fullLowerBound(param) else fullUpperBound(param)
        typr.println(s"approx ${param.show}, from below = $fromBelow, inst = ${inst.show}")
        inst
      case inst =>
        assert(inst.exists, i"param = $param\nconstraint = $constraint")
        inst
  end approximation

  /** If `tp` is an intersection such that some operands are transparent trait instances
   *  and others are not, replace as many transparent trait instances as possible with Any
   *  as long as the result is still a subtype of `bound`. But fall back to the
   *  original type if the resulting widened type is a supertype of all dropped
   *  types (since in this case the type was not a true intersection of transparent traits
   *  and other types to start with).
   */
  def dropTransparentTraits(tp: Type, bound: Type)(using Context): Type =
    var kept: Set[Type] = Set()      // types to keep since otherwise bound would not fit
    var dropped: List[Type] = List() // the types dropped so far, last one on top

    def dropOneTransparentTrait(tp: Type): Type =
      val tpd = tp.dealias
      if tpd.typeSymbol.isTransparentTrait && !tpd.isLambdaSub && !kept.contains(tpd) then
        dropped = tpd :: dropped
        defn.AnyType
      else tpd match
        case AndType(tp1, tp2) =>
          val tp1w = dropOneTransparentTrait(tp1)
          if tp1w ne tp1 then tp1w & tp2
          else
            val tp2w = dropOneTransparentTrait(tp2)
            if tp2w ne tp2 then tp1 & tp2w
            else tpd
        case _ =>
          tp

    def recur(tp: Type): Type =
      val tpw = dropOneTransparentTrait(tp)
      if tpw eq tp then tp
      else if tpw <:< bound then recur(tpw)
      else
        kept += dropped.head
        dropped = dropped.tail
        recur(tp)

    val saved = ctx.typerState.snapshot()
    val tpw = recur(tp)
    if (tpw eq tp) || dropped.forall(_ frozen_<:< tpw) then
      // Rollback any constraint change that would lead to `tp` no longer
      // being a valid solution.
      ctx.typerState.resetTo(saved)
      tp
    else
      tpw
  end dropTransparentTraits

  /** If `tp` is an applied match type alias which is also an unreducible application
   *  of a higher-kinded type to a wildcard argument, widen to the match type's bound,
   *  in order to avoid an unreducible application of higher-kinded type ... in inferred type"
   *  error in PostTyper. Fixes #11246.
   */
  def widenIrreducible(tp: Type)(using Context): Type = tp match
    case tp @ AppliedType(tycon, _) if tycon.isLambdaSub && tp.hasWildcardArg =>
      tp.superType match
        case MatchType(bound, _, _) => bound
        case _ => tp
    case _ =>
      tp

  /** Widen inferred type `inst` with upper `bound`, according to the following rules:
   *   1. If `inst` is a singleton type, or a union containing some singleton types,
   *      widen (all) the singleton type(s), provided the result is a subtype of `bound`.
   *      (i.e. `inst.widenSingletons <:< bound` succeeds with satisfiable constraint)
   *   2. If `inst` is a union type, approximate the union type from above by an intersection
   *      of all common base types, provided the result is a subtype of `bound`.
   *   3. Widen some irreducible applications of higher-kinded types to wildcard arguments
   *      (see @widenIrreducible).
   *   4. Drop transparent traits from intersections (see @dropTransparentTraits).
   *
   *  Don't do these widenings if `bound` is a subtype of `scala.Singleton`.
   *  Also, if the result of these widenings is a TypeRef to a module class,
   *  and this type ref is different from `inst`, replace by a TermRef to
   *  its source module instead.
   *
   * At this point we also drop the @Repeated annotation to avoid inferring type arguments with it,
   * as those could leak the annotation to users (see run/inferred-repeated-result).
   */
  def widenInferred(inst: Type, bound: Type)(using Context): Type =
    def widenOr(tp: Type) =
      val tpw = tp.widenUnion
      if (tpw ne tp) && (tpw <:< bound) then tpw else tp

    def widenSingle(tp: Type) =
      val tpw = tp.widenSingletons
      if (tpw ne tp) && (tpw <:< bound) then tpw else tp

    def isSingleton(tp: Type): Boolean = tp match
      case WildcardType(optBounds) => optBounds.exists && isSingleton(optBounds.bounds.hi)
      case _ => isSubTypeWhenFrozen(tp, defn.SingletonType)

    val wideInst =
      if isSingleton(bound) then inst
      else dropTransparentTraits(widenIrreducible(widenOr(widenSingle(inst))), bound)
    wideInst match
      case wideInst: TypeRef if wideInst.symbol.is(Module) =>
        TermRef(wideInst.prefix, wideInst.symbol.sourceModule)
      case _ =>
        wideInst.dropRepeatedAnnot
  end widenInferred

  /** The instance type of `param` in the current constraint (which contains `param`).
   *  If `fromBelow` is true, the instance type is the lub of the parameter's
   *  lower bounds; otherwise it is the glb of its upper bounds. However,
   *  a lower bound instantiation can be a singleton type only if the upper bound
   *  is also a singleton type.
   */
  def instanceType(param: TypeParamRef, fromBelow: Boolean)(using Context): Type = {
    val approx = approximation(param, fromBelow).simplified
    if fromBelow then
      val widened = widenInferred(approx, param)
      // Widening can add extra constraints, in particular the widened type might
      // be a type variable which is now instantiated to `param`, and therefore
      // cannot be used as an instantiation of `param` without creating a loop.
      // If that happens, we run `instanceType` again to find a new instantation.
      // (we do not check for non-toplevel occurences: those should never occur
      // since `addOneBound` disallows recursive lower bounds).
      if constraint.occursAtToplevel(param, widened) then
        instanceType(param, fromBelow)
      else
        widened
    else
      approx
  }

  /** Constraint `c1` subsumes constraint `c2`, if under `c2` as constraint we have
   *  for all poly params `p` defined in `c2` as `p >: L2 <: U2`:
   *
   *     c1 defines p with bounds p >: L1 <: U1, and
   *     L2 <: L1, and
   *     U1 <: U2
   *
   *  Both `c1` and `c2` are required to derive from constraint `pre`, without adding
   *  any new type variables but possibly narrowing already registered ones with further bounds.
   */
  protected final def subsumes(c1: Constraint, c2: Constraint, pre: Constraint)(using Context): Boolean =
    if (c2 eq pre) true
    else if (c1 eq pre) false
    else {
      val saved = constraint
      try
        // We iterate over params of `pre`, instead of `c2` as the documentation may suggest.
        // As neither `c1` nor `c2` can have more params than `pre`, this only matters in one edge case.
        // Constraint#forallParams only iterates over params that can be directly constrained.
        // If `c2` has, compared to `pre`, instantiated a param and we iterated over params of `c2`,
        // we could miss that param being instantiated to an incompatible type in `c1`.
        pre.forallParams(p =>
          c1.entry(p).exists
          && c2.upper(p).forall(c1.isLess(p, _))
          && isSubTypeWhenFrozen(c1.nonParamBounds(p), c2.nonParamBounds(p))
        )
      finally constraint = saved
    }

  /** The current bounds of type parameter `param` */
  def bounds(param: TypeParamRef)(using Context): TypeBounds = {
    val e = constraint.entry(param)
    if (e.exists) e.bounds
    else {
      val pinfos = param.binder.paramInfos
      if (pinfos != null) pinfos(param.paramNum) // pinfos == null happens in pos/i536.scala
      else TypeBounds.empty
    }
  }

  /** Add type lambda `tl`, possibly with type variables `tvars`, to current constraint
   *  and propagate all bounds.
   *  @param tvars   See Constraint#add
   */
  def addToConstraint(tl: TypeLambda, tvars: List[TypeVar])(using Context): Boolean =
    checkPropagated(i"initialized $tl") {
      constraint = constraint.add(tl, tvars)
      tl.paramRefs.forall { param =>
        val lower = constraint.lower(param)
        val upper = constraint.upper(param)
        constraint.entry(param) match {
          case bounds: TypeBounds =>
            if lower.nonEmpty && !bounds.lo.isRef(defn.NothingClass)
               || upper.nonEmpty && !bounds.hi.isAny
            then constr.println(i"INIT*** $tl")
            lower.forall(addOneBound(_, bounds.hi, isUpper = true)) &&
              upper.forall(addOneBound(_, bounds.lo, isUpper = false))
          case x =>
            // Happens if param was already solved while processing earlier params of the same TypeLambda.
            // See #4720.

            // Should propagate bounds even when param has been solved.
            // See #11682.
            lower.forall(addOneBound(_, x, isUpper = true)) &&
              upper.forall(addOneBound(_, x, isUpper = false))
        }
      }
    }

  /** Can `param` be constrained with new bounds? */
  final def canConstrain(param: TypeParamRef): Boolean =
    (!frozenConstraint || (caseLambda `eq` param.binder)) && constraint.contains(param)

  /** Is `param` assumed to be a sub- and super-type of any other type?
   *  This holds if `TypeVarsMissContext` is set unless `param` is a part
   *  of a MatchType that is currently normalized.
   */
  final def assumedTrue(param: TypeParamRef)(using Context): Boolean =
    ctx.mode.is(Mode.TypevarsMissContext) && (caseLambda `ne` param.binder)

  /** Add constraint `param <: bound` if `fromBelow` is false, `param >: bound` otherwise.
   *  `bound` is assumed to be in normalized form, as specified in `firstTry` and
   *  `secondTry` of `TypeComparer`. In particular, it should not be an alias type,
   *  lazy ref, typevar, wildcard type, error type. In addition, upper bounds may
   *  not be AndTypes and lower bounds may not be OrTypes. This is assured by the
   *  way isSubType is organized.
   */
  protected def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean)(using Context): Boolean =
    if !bound.isValueTypeOrLambda then return false

    /** When comparing lambdas we might get constraints such as
     *  `A <: X0` or `A = List[X0]` where `A` is a constrained parameter
     *  and `X0` is a lambda parameter. The constraint for `A` is not allowed
     *  to refer to such a lambda parameter because the lambda parameter is
     *  not visible where `A` is defined. Consequently, we need to
     *  approximate the bound so that the lambda parameter does not appear in it.
     *  If `tp` is an upper bound, we need to approximate with something smaller,
     *  otherwise something larger.
     *  Test case in pos/i94-nada.scala. This test crashes with an illegal instance
     *  error in Test2 when the rest of the SI-2712 fix is applied but `pruneLambdaParams` is
     *  missing.
     */
    def avoidLambdaParams(tp: Type) =
      if comparedTypeLambdas.nonEmpty then
        val approx = new ApproximatingTypeMap {
          if (!fromBelow) variance = -1
          def apply(t: Type): Type = t match {
            case t @ TypeParamRef(tl: TypeLambda, n) if comparedTypeLambdas contains tl =>
              val bounds = tl.paramInfos(n)
              range(bounds.lo, bounds.hi)
            case _ =>
              mapOver(t)
          }
        }
        approx(tp)
      else tp

    def addParamBound(bound: TypeParamRef) =
      constraint.entry(param) match {
        case _: TypeBounds =>
          if (fromBelow) addLess(bound, param) else addLess(param, bound)
        case tp =>
          if (fromBelow) isSub(bound, tp) else isSub(tp, bound)
      }

    def kindCompatible(tp1: Type, tp2: Type): Boolean =
      val tparams1 = tp1.typeParams
      val tparams2 = tp2.typeParams
      tparams1.corresponds(tparams2)((p1, p2) => kindCompatible(p1.paramInfo, p2.paramInfo))
      && (tparams1.isEmpty || kindCompatible(tp1.hkResult, tp2.hkResult))
      || tp1.hasAnyKind
      || tp2.hasAnyKind

    def description = i"constr $param ${if (fromBelow) ">:" else "<:"} $bound:\n$constraint"

    //checkPropagated(s"adding $description")(true) // DEBUG in case following fails
    checkPropagated(s"added $description") {
      addConstraintInvocations += 1
      try bound match
        case bound: TypeParamRef if constraint contains bound =>
          addParamBound(bound)
        case _ =>
          val pbound = avoidLambdaParams(bound)
          kindCompatible(param, pbound) && addBoundTransitively(param, pbound, !fromBelow)
      finally addConstraintInvocations -= 1
    }
  end addConstraint

  /** Check that constraint is fully propagated. See comment in Config.checkConstraintsPropagated */
  def checkPropagated(msg: => String)(result: Boolean)(using Context): Boolean = {
    if (Config.checkConstraintsPropagated && result && addConstraintInvocations == 0)
      inFrozenConstraint {
        for (p <- constraint.domainParams) {
          def check(cond: => Boolean, q: TypeParamRef, ordering: String, explanation: String): Unit =
            assert(cond, i"propagation failure for $p $ordering $q: $explanation\n$msg")
          for (u <- constraint.upper(p))
            check(bounds(p).hi <:< bounds(u).hi, u, "<:", "upper bound not propagated")
          for (l <- constraint.lower(p)) {
            check(bounds(l).lo <:< bounds(p).hi, l, ">:", "lower bound not propagated")
            check(constraint.isLess(l, p), l, ">:", "reverse ordering (<:) missing")
          }
        }
      }
    result
  }
}
