package dotty.tools
package dotc
package core

import Types._
import Contexts._
import Symbols._
import Decorators._
import config.Config
import config.Printers.{constr, typr}
import dotty.tools.dotc.reporting.trace

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
trait ConstraintHandling[AbstractContext] {

  def constr_println(msg: => String): Unit = constr.println(msg)
  def typr_println(msg: => String): Unit = typr.println(msg)

  implicit def ctx(implicit ac: AbstractContext): Context

  protected def isSubType(tp1: Type, tp2: Type)(implicit actx: AbstractContext): Boolean
  protected def isSameType(tp1: Type, tp2: Type)(implicit actx: AbstractContext): Boolean

  protected def constraint: Constraint
  protected def constraint_=(c: Constraint): Unit

  private[this] var addConstraintInvocations = 0

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

  /** Gives for each instantiated type var that does not yet have its `inst` field
    *  set, the instance value stored in the constraint. Storing instances in constraints
    *  is done only in a temporary way for contexts that may be retracted
    *  without also retracting the type var as a whole.
    */
  def instType(tvar: TypeVar): Type = constraint.entry(tvar.origin) match {
    case _: TypeBounds => NoType
    case tp: TypeParamRef =>
      var tvar1 = constraint.typeVarOfParam(tp)
      if (tvar1.exists) tvar1 else tp
    case tp => tp
  }

  def nonParamBounds(param: TypeParamRef)(implicit actx: AbstractContext): TypeBounds = constraint.nonParamBounds(param)

  def fullLowerBound(param: TypeParamRef)(implicit actx: AbstractContext): Type =
    (nonParamBounds(param).lo /: constraint.minLower(param))(_ | _)

  def fullUpperBound(param: TypeParamRef)(implicit actx: AbstractContext): Type =
    (nonParamBounds(param).hi /: constraint.minUpper(param))(_ & _)

  /** Full bounds of `param`, including other lower/upper params.
    *
    * Note that underlying operations perform subtype checks - for this reason, recursing on `fullBounds`
    * of some param when comparing types might lead to infinite recursion. Consider `bounds` instead.
    */
  def fullBounds(param: TypeParamRef)(implicit actx: AbstractContext): TypeBounds =
    nonParamBounds(param).derivedTypeBounds(fullLowerBound(param), fullUpperBound(param))

  protected def addOneBound(param: TypeParamRef, bound: Type, isUpper: Boolean)(implicit actx: AbstractContext): Boolean =
    !constraint.contains(param) || {
      def occursIn(bound: Type): Boolean = {
        val b = bound.dealias
        (b eq param) || {
          b match {
            case b: AndType => occursIn(b.tp1) || occursIn(b.tp2)
            case b: OrType  => occursIn(b.tp1) || occursIn(b.tp2)
            case b: TypeVar => occursIn(b.origin)
            case b: TermRef => occursIn(b.underlying)
            case _ => false
          }
        }
      }
      if (Config.checkConstraintsSeparated)
        assert(!occursIn(bound), s"$param occurs in $bound")

      val oldBounds @ TypeBounds(lo, hi) = constraint.nonParamBounds(param)
      val equalBounds = isUpper && (lo eq bound) || !isUpper && (bound eq hi)
      if (equalBounds &&
         !bound.existsPart(bp => bp.isInstanceOf[WildcardType] || (bp eq param))) {
        // The narrowed bounds are equal and do not contain wildcards,
        // so we can remove `param` from the constraint.
        // (Handling wildcards requires choosing a bound, but we don't know which
        // bound to choose here, this is handled in `ConstraintHandling#approximation`)
        constraint = constraint.replace(param, bound)
        true
      }
      else {
        // Narrow one of the bounds of type parameter `param`
        // If `isUpper` is true, ensure that `param <: `bound`, otherwise ensure
        // that `param >: bound`.
        val narrowedBounds = {
          val saved = homogenizeArgs
          homogenizeArgs = Config.alignArgsInAnd
          try
            if (isUpper) oldBounds.derivedTypeBounds(lo, hi & bound)
            else oldBounds.derivedTypeBounds(lo | bound, hi)
          finally homogenizeArgs = saved
        }
        val c1 = constraint.updateEntry(param, narrowedBounds)
        (c1 eq constraint) || {
          constraint = c1
          val TypeBounds(lo, hi) = constraint.entry(param)
          isSubType(lo, hi)
        }
      }
    }

  private def location(implicit ctx: Context) = "" // i"in ${ctx.typerState.stateChainStr}" // use for debugging

  protected def addUpperBound(param: TypeParamRef, bound: Type)(implicit actx: AbstractContext): Boolean = {
    def description = i"constraint $param <: $bound to\n$constraint"
    if (bound.isRef(defn.NothingClass) && ctx.typerState.isGlobalCommittable) {
      def msg = s"!!! instantiated to Nothing: $param, constraint = ${constraint.show}"
      if (Config.failOnInstantiationToNothing) assert(false, msg)
      else ctx.log(msg)
    }
    constr_println(i"adding $description$location")
    val lower = constraint.lower(param)
    val res =
      addOneBound(param, bound, isUpper = true) &&
      lower.forall(addOneBound(_, bound, isUpper = true))
    constr_println(i"added $description = $res$location")
    res
  }

  protected def addLowerBound(param: TypeParamRef, bound: Type)(implicit actx: AbstractContext): Boolean = {
    def description = i"constraint $param >: $bound to\n$constraint"
    constr_println(i"adding $description")
    val upper = constraint.upper(param)
    val res =
      addOneBound(param, bound, isUpper = false) &&
      upper.forall(addOneBound(_, bound, isUpper = false))
    constr_println(i"added $description = $res$location")
    res
  }

  protected def addLess(p1: TypeParamRef, p2: TypeParamRef)(implicit actx: AbstractContext): Boolean = {
    def description = i"ordering $p1 <: $p2 to\n$constraint"
    val res =
      if (constraint.isLess(p2, p1)) unify(p2, p1)
      else {
        val down1 = p1 :: constraint.exclusiveLower(p1, p2)
        val up2 = p2 :: constraint.exclusiveUpper(p2, p1)
        val lo1 = constraint.nonParamBounds(p1).lo
        val hi2 = constraint.nonParamBounds(p2).hi
        constr_println(i"adding $description down1 = $down1, up2 = $up2$location")
        constraint = constraint.addLess(p1, p2)
        down1.forall(addOneBound(_, hi2, isUpper = true)) &&
        up2.forall(addOneBound(_, lo1, isUpper = false))
      }
    constr_println(i"added $description = $res$location")
    res
  }

  /** Make p2 = p1, transfer all bounds of p2 to p1
   *  @pre  less(p1)(p2)
   */
  private def unify(p1: TypeParamRef, p2: TypeParamRef)(implicit actx: AbstractContext): Boolean = {
    constr_println(s"unifying $p1 $p2")
    assert(constraint.isLess(p1, p2))
    val down = constraint.exclusiveLower(p2, p1)
    val up = constraint.exclusiveUpper(p1, p2)
    constraint = constraint.unify(p1, p2)
    val bounds = constraint.nonParamBounds(p1)
    val lo = bounds.lo
    val hi = bounds.hi
    isSubType(lo, hi) &&
    down.forall(addOneBound(_, hi, isUpper = true)) &&
    up.forall(addOneBound(_, lo, isUpper = false))
  }


  protected def isSubType(tp1: Type, tp2: Type, whenFrozen: Boolean)(implicit actx: AbstractContext): Boolean = {
    if (whenFrozen)
      isSubTypeWhenFrozen(tp1, tp2)
    else
      isSubType(tp1, tp2)
  }

  @forceInline final def inFrozenConstraint[T](op: => T): T = {
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

  final def isSubTypeWhenFrozen(tp1: Type, tp2: Type)(implicit actx: AbstractContext): Boolean = inFrozenConstraint(isSubType(tp1, tp2))
  final def isSameTypeWhenFrozen(tp1: Type, tp2: Type)(implicit actx: AbstractContext): Boolean = inFrozenConstraint(isSameType(tp1, tp2))

  /** Test whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   */
  protected final def isSatisfiable(implicit actx: AbstractContext): Boolean =
    constraint.forallParams { param =>
      val TypeBounds(lo, hi) = constraint.entry(param)
      isSubType(lo, hi) || {
        ctx.log(i"sub fail $lo <:< $hi")
        false
      }
    }

  /** Solve constraint set for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound. However, any occurrences
   *  of the parameter in a refinement somewhere in the bound are removed. Also
   *  wildcard types in bounds are approximated by their upper or lower bounds.
   *  (Such occurrences can arise for F-bounded types).
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  final def approximation(param: TypeParamRef, fromBelow: Boolean)(implicit actx: AbstractContext): Type = {
    val avoidParam = new TypeMap {
      override def stopAtStatic = true
      def avoidInArg(arg: Type): Type =
        if (param.occursIn(arg)) TypeBounds.empty else arg
      def apply(tp: Type) = mapOver {
        tp match {
          case tp @ AppliedType(tycon, args) =>
            tp.derivedAppliedType(tycon, args.mapConserve(avoidInArg))
          case tp: RefinedType if param occursIn tp.refinedInfo =>
            tp.parent
          case tp: WildcardType =>
            val bounds = tp.optBounds.orElse(TypeBounds.empty).bounds
            // Try to instantiate the wildcard to a type that is known to conform to it.
            // This means:
            //  If fromBelow is true, we minimize the type overall
            //  Hence, if variance < 0, pick the maximal safe type: bounds.lo
            //           (i.e. the whole bounds range is over the type)
            //         if variance > 0, pick the minimal safe type: bounds.hi
            //           (i.e. the whole bounds range is under the type)
            //         if variance == 0, pick bounds.lo anyway (this is arbitrary but in line with
            //           the principle that we pick the smaller type when in doubt).
            //  If fromBelow is false, we maximize the type overall and reverse the bounds
            //  if variance != 0. For variance == 0, we still minimize.
            //  In summary we pick the bound given by this table:
            //
            //  variance    | -1  0   1
            //  ------------------------
            //  from below  | lo  lo  hi
            //  from above  | hi  lo  lo
            //
            if (variance == 0 || fromBelow == (variance < 0)) bounds.lo else bounds.hi
          case _ => tp
        }
      }
    }
    constraint.entry(param) match {
      case _: TypeBounds =>
        val bound = if (fromBelow) fullLowerBound(param) else fullUpperBound(param)
        val inst = avoidParam(bound)
        typr_println(s"approx ${param.show}, from below = $fromBelow, bound = ${bound.show}, inst = ${inst.show}")
        inst
      case inst =>
        assert(inst.exists, i"param = $param\nconstraint = $constraint")
        inst
    }
  }

  /** Widen inferred type `inst` with upper `bound`, according to the following rules:
   *   1. If `inst` is a singleton type, or a union containing some singleton types,
   *      widen (all) the singleton type(s), provied the result is a subtype of `bound`
   *      (i.e. `inst.widenSingletons <:< bound` succeeds with satisfiable constraint)
   *   2. If `inst` is a union type, approximate the union type from above by an intersection
   *      of all common base types, provied the result is a subtype of `bound`.
   *
   *  Don't do these widenings if `bound` is a subtype of `scala.Singleton`.
   *
   * At this point we also drop the @Repeated annotation to avoid inferring type arguments with it,
   * as those could leak the annotation to users (see run/inferred-repeated-result).
   */
  def widenInferred(inst: Type, bound: Type)(implicit actx: AbstractContext): Type = {
    def widenOr(tp: Type) = {
      val tpw = tp.widenUnion
      if ((tpw ne tp) && tpw <:< bound) tpw else tp
    }
    def widenSingle(tp: Type) = {
      val tpw = tp.widenSingletons
      if ((tpw ne tp) && tpw <:< bound) tpw else tp
    }
    val wideInst =
      if (isSubTypeWhenFrozen(bound, defn.SingletonType)) inst
      else widenOr(widenSingle(inst))
    wideInst.dropRepeatedAnnot
  }

  /** The instance type of `param` in the current constraint (which contains `param`).
   *  If `fromBelow` is true, the instance type is the lub of the parameter's
   *  lower bounds; otherwise it is the glb of its upper bounds. However,
   *  a lower bound instantiation can be a singleton type only if the upper bound
   *  is also a singleton type.
   */
  def instanceType(param: TypeParamRef, fromBelow: Boolean)(implicit actx: AbstractContext): Type = {
    val inst = approximation(param, fromBelow).simplified
    if (fromBelow) widenInferred(inst, param) else inst
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
  protected final def subsumes(c1: Constraint, c2: Constraint, pre: Constraint)(implicit actx: AbstractContext): Boolean =
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
          c1.contains(p) &&
          c2.upper(p).forall(c1.isLess(p, _)) &&
          isSubTypeWhenFrozen(c1.nonParamBounds(p), c2.nonParamBounds(p)))
      finally constraint = saved
    }

  /** The current bounds of type parameter `param` */
  def bounds(param: TypeParamRef)(implicit actx: AbstractContext): TypeBounds = {
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
  def addToConstraint(tl: TypeLambda, tvars: List[TypeVar])(implicit actx: AbstractContext): Boolean =
    checkPropagated(i"initialized $tl") {
      constraint = constraint.add(tl, tvars)
      tl.paramRefs.forall { param =>
        constraint.entry(param) match {
          case bounds: TypeBounds =>
            val lower = constraint.lower(param)
            val upper = constraint.upper(param)
            if (lower.nonEmpty && !bounds.lo.isRef(defn.NothingClass) ||
              upper.nonEmpty && !bounds.hi.isRef(defn.AnyClass)) constr_println(i"INIT*** $tl")
            lower.forall(addOneBound(_, bounds.hi, isUpper = true)) &&
              upper.forall(addOneBound(_, bounds.lo, isUpper = false))
          case _ =>
            // Happens if param was already solved while processing earlier params of the same TypeLambda.
            // See #4720.
            true
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
  final def assumedTrue(param: TypeParamRef)(implicit actx: AbstractContext): Boolean =
    ctx.mode.is(Mode.TypevarsMissContext) && (caseLambda `ne` param.binder)

  /** Add constraint `param <: bound` if `fromBelow` is false, `param >: bound` otherwise.
   *  `bound` is assumed to be in normalized form, as specified in `firstTry` and
   *  `secondTry` of `TypeComparer`. In particular, it should not be an alias type,
   *  lazy ref, typevar, wildcard type, error type. In addition, upper bounds may
   *  not be AndTypes and lower bounds may not be OrTypes. This is assured by the
   *  way isSubType is organized.
   */
  protected def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean)(implicit actx: AbstractContext): Boolean = {
    def description = i"constr $param ${if (fromBelow) ">:" else "<:"} $bound:\n$constraint"
    //checkPropagated(s"adding $description")(true) // DEBUG in case following fails
    checkPropagated(s"added $description") {
      addConstraintInvocations += 1

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
      def pruneLambdaParams(tp: Type) =
        if (comparedTypeLambdas.nonEmpty) {
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
        }
        else tp

      def addParamBound(bound: TypeParamRef) =
        constraint.entry(param) match {
          case _: TypeBounds =>
            if (fromBelow) addLess(bound, param) else addLess(param, bound)
          case tp =>
            if (fromBelow) isSubType(bound, tp) else isSubType(tp, bound)
        }

      /** Drop all constrained parameters that occur at the toplevel in `bound` and
       *  handle them by `addLess` calls.
       *  The preconditions make sure that such parameters occur only
       *  in one of two ways:
       *
       *  1.
       *
       *    P <: Ts1 | ... | Tsm   (m > 0)
       *    Tsi = T1 & ... Tn      (n >= 0)
       *    Some of the Ti are constrained parameters
       *
       *  2.
       *
       *    Ts1 & ... & Tsm <: P   (m > 0)
       *    Tsi = T1 | ... | Tn    (n >= 0)
       *    Some of the Ti are constrained parameters
       *
       *  In each case we cannot leave the parameter in place,
       *  because that would risk making a parameter later a subtype or supertype
       *  of a bound where the parameter occurs again at toplevel, which leads to cycles
       *  in the subtyping test. So we intentionally narrow the constraint by
       *  recording an isLess relationship instead (even though this is not implied
       *  by the bound).
       *
       *  Narrowing a constraint is better than widening it, because narrowing leads
       *  to incompleteness (which we face anyway, see for instance eitherIsSubType)
       *  but widening leads to unsoundness.
       *
       *  A test case that demonstrates the problem is i864.scala.
       *  Turn Config.checkConstraintsSeparated on to get an accurate diagnostic
       *  of the cycle when it is created.
       *
       *  @return The pruned type if all `addLess` calls succeed, `NoType` otherwise.
       */
      def prune(bound: Type): Type = bound match {
        case bound: AndType =>
          val p1 = prune(bound.tp1)
          val p2 = prune(bound.tp2)
          if (p1.exists && p2.exists) bound.derivedAndType(p1, p2)
          else NoType
        case bound: OrType =>
          val p1 = prune(bound.tp1)
          val p2 = prune(bound.tp2)
          if (p1.exists && p2.exists) bound.derivedOrType(p1, p2)
          else NoType
        case bound: TypeVar if constraint contains bound.origin =>
          prune(bound.underlying)
        case bound: TypeParamRef =>
          constraint.entry(bound) match {
            case NoType => pruneLambdaParams(bound)
            case _: TypeBounds =>
              if (!addParamBound(bound)) NoType
              else if (fromBelow) defn.NothingType
              else defn.AnyType
            case inst =>
              prune(inst)
          }
        case _ =>
          pruneLambdaParams(bound)
      }

      try bound match {
        case bound: TypeParamRef if constraint contains bound =>
          addParamBound(bound)
        case _ =>
          val pbound = prune(bound)
          pbound.exists && (
            if (fromBelow) addLowerBound(param, pbound) else addUpperBound(param, pbound))
      }
      finally addConstraintInvocations -= 1
    }
  }

  /** Check that constraint is fully propagated. See comment in Config.checkConstraintsPropagated */
  def checkPropagated(msg: => String)(result: Boolean)(implicit actx: AbstractContext): Boolean = {
    if (Config.checkConstraintsPropagated && result && addConstraintInvocations == 0) {
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
    }
    result
  }
}
