package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._
import Decorators._
import config.Config
import config.Printers._
import collection.mutable

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

  implicit val ctx: Context

  protected def isSubType(tp1: Type, tp2: Type): Boolean
  protected def isSameType(tp1: Type, tp2: Type): Boolean

  val state: TyperState
  import state.constraint

  private var addConstraintInvocations = 0

  /** If the constraint is frozen we cannot add new bounds to the constraint. */
  protected var frozenConstraint = false

  private def addOneBound(param: PolyParam, bound: Type, isUpper: Boolean): Boolean =
    !constraint.contains(param) || {
      def occursIn(bound: Type): Boolean = {
        val b = bound.dealias
        (b eq param) || {
          b match {
            case b: AndOrType => occursIn(b.tp1) || occursIn(b.tp2)
            case b: TypeVar => occursIn(b.origin)
            case _ => false
          }
        }
      }
      if (Config.checkConstraintsSeparated)
        assert(!occursIn(bound), s"$param occurs in $bound")
      val c1 = constraint.narrowBound(param, bound, isUpper)
      (c1 eq constraint) || {
        constraint = c1
        val TypeBounds(lo, hi) = constraint.entry(param)
        isSubType(lo, hi)
      }
    }

  protected def addUpperBound(param: PolyParam, bound: Type): Boolean = {
    def description = i"constraint $param <: $bound to\n$constraint"
    if (bound.isRef(defn.NothingClass) && ctx.typerState.isGlobalCommittable) {
      def msg = s"!!! instantiated to Nothing: $param, constraint = ${constraint.show}"
      if (Config.failOnInstantiationToNothing) assert(false, msg)
      else ctx.log(msg)
    }
    constr.println(i"adding $description")
    val lower = constraint.lower(param)
    val res =
      addOneBound(param, bound, isUpper = true) &&
      lower.forall(addOneBound(_, bound, isUpper = true))
    constr.println(i"added $description = $res")
    res
  }

  protected def addLowerBound(param: PolyParam, bound: Type): Boolean = {
    def description = i"constraint $param >: $bound to\n$constraint"
    constr.println(i"adding $description")
    val upper = constraint.upper(param)
    val res =
      addOneBound(param, bound, isUpper = false) &&
      upper.forall(addOneBound(_, bound, isUpper = false))
    constr.println(i"added $description = $res")
    res
  }

  protected def addLess(p1: PolyParam, p2: PolyParam): Boolean = {
    def description = i"ordering $p1 <: $p2 to\n$constraint"
    val res =
      if (constraint.isLess(p2, p1)) unify(p2, p1)
      else {
        val down1 = p1 :: constraint.exclusiveLower(p1, p2)
        val up2 = p2 :: constraint.exclusiveUpper(p2, p1)
        val lo1 = constraint.nonParamBounds(p1).lo
        val hi2 = constraint.nonParamBounds(p2).hi
        constr.println(i"adding $description down1 = $down1, up2 = $up2")
        constraint = constraint.addLess(p1, p2)
        down1.forall(addOneBound(_, hi2, isUpper = true)) &&
        up2.forall(addOneBound(_, lo1, isUpper = false))
      }
    constr.println(i"added $description = $res")
    res
  }

  /** Make p2 = p1, transfer all bounds of p2 to p1
   *  @pre  less(p1)(p2)
   */
  private def unify(p1: PolyParam, p2: PolyParam): Boolean = {
    constr.println(s"unifying $p1 $p2")
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

  final def isSubTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    val saved = frozenConstraint
    frozenConstraint = true
    try isSubType(tp1, tp2)
    finally frozenConstraint = saved
  }

  final def isSameTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    val saved = frozenConstraint
    frozenConstraint = true
    try isSameType(tp1, tp2)
    finally frozenConstraint = saved
  }

  /** Test whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   */
  protected final def isSatisfiable: Boolean =
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
   *  of the parameter in a refinement somewhere in the bound are removed.
   *  (Such occurrences can arise for F-bounded types).
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  final def approximation(param: PolyParam, fromBelow: Boolean): Type = {
    val avoidParam = new TypeMap {
      override def stopAtStatic = true
      def apply(tp: Type) = mapOver {
        tp match {
          case tp: RefinedType if param occursIn tp.refinedInfo => tp.parent
          case _ => tp
        }
      }
    }
    val bound = if (fromBelow) constraint.fullLowerBound(param) else constraint.fullUpperBound(param)
    val inst = avoidParam(bound)
    typr.println(s"approx ${param.show}, from below = $fromBelow, bound = ${bound.show}, inst = ${inst.show}")
    inst
  }

  /** Constraint `c1` subsumes constraint `c2`, if under `c2` as constraint we have
   *  for all poly params `p` defined in `c2` as `p >: L2 <: U2`:
   *
   *     c1 defines p with bounds p >: L1 <: U1, and
   *     L2 <: L1, and
   *     U1 <: U2
   *
   *  Both `c1` and `c2` are required to derive from constraint `pre`, possibly
   *  narrowing it with further bounds.
   */
  protected final def subsumes(c1: Constraint, c2: Constraint, pre: Constraint): Boolean =
    if (c2 eq pre) true
    else if (c1 eq pre) false
    else {
      val saved = constraint
      try
        c2.forallParams(p =>
          c1.contains(p) &&
          c2.upper(p).forall(c1.isLess(p, _)) &&
          isSubTypeWhenFrozen(c1.nonParamBounds(p), c2.nonParamBounds(p)))
      finally constraint = saved
    }

  /** The current bounds of type parameter `param` */
  final def bounds(param: PolyParam): TypeBounds = constraint.entry(param) match {
    case bounds: TypeBounds => bounds
    case _ => param.binder.paramBounds(param.paramNum)
  }

  /** Add polytype `pt`, possibly with type variables `tvars`, to current constraint
   *  and propagate all bounds.
   *  @param tvars   See Constraint#add
   */
  def addToConstraint(pt: PolyType, tvars: List[TypeVar]): Unit =
    assert {
      checkPropagated(i"initialized $pt") {
        constraint = constraint.add(pt, tvars)
        pt.paramNames.indices.forall { i =>
          val param = PolyParam(pt, i)
          val bounds = constraint.nonParamBounds(param)
          val lower = constraint.lower(param)
          val upper = constraint.upper(param)
          if (lower.nonEmpty && !bounds.lo.isRef(defn.NothingClass) ||
            upper.nonEmpty && !bounds.hi.isRef(defn.AnyClass)) constr.println(i"INIT*** $pt")
          lower.forall(addOneBound(_, bounds.hi, isUpper = true)) &&
            upper.forall(addOneBound(_, bounds.lo, isUpper = false))
        }
      }
    }

  /** Can `param` be constrained with new bounds? */
  final def canConstrain(param: PolyParam): Boolean =
    !frozenConstraint && (constraint contains param)

  /** Add constraint `param <: bound` if `fromBelow` is false, `param >: bound` otherwise.
   *  `bound` is assumed to be in normalized form, as specified in `firstTry` and
   *  `secondTry` of `TypeComparer`. In particular, it should not be an alias type,
   *  lazy ref, typevar, wildcard type, error type. In addition, upper bounds may
   *  not be AndTypes and lower bounds may not be OrTypes. This is assured by the
   *  way isSubType is organized.
   */
  protected def addConstraint(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean = {
    def description = i"constr $param ${if (fromBelow) ">:" else "<:"} $bound:\n$constraint"
    //checkPropagated(s"adding $description")(true) // DEBUG in case following fails
    checkPropagated(s"added $description") {
      addConstraintInvocations += 1

      def addParamBound(bound: PolyParam) =
        if (fromBelow) addLess(bound, param) else addLess(param, bound)

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
        case bound: AndOrType =>
          val p1 = prune(bound.tp1)
          val p2 = prune(bound.tp2)
          if (p1.exists && p2.exists) bound.derivedAndOrType(p1, p2)
          else NoType
        case bound: TypeVar if constraint contains bound.origin =>
          prune(bound.underlying)
        case bound: PolyParam if constraint contains bound =>
          if (!addParamBound(bound)) NoType
          else if (fromBelow) defn.NothingType
          else defn.AnyType
        case bound: RefinedType =>
          bound.normalizeHkApply
        case _ =>
          bound
      }

      try bound match {
        case bound: PolyParam if constraint contains bound =>
          addParamBound(bound)
        case _ =>
          val pbound = prune(bound)
          pbound.exists && (
            if (fromBelow) addLowerBound(param, pbound) else addUpperBound(param, pbound))
      }
      finally addConstraintInvocations -= 1
    }
  }

  /** Instantiate `param` to `tp` if the constraint stays satisfiable */
  protected def tryInstantiate(param: PolyParam, tp: Type): Boolean = {
    val saved = constraint
    constraint =
      if (addConstraint(param, tp, fromBelow = true) &&
          addConstraint(param, tp, fromBelow = false)) constraint.replace(param, tp)
      else saved
    constraint ne saved
  }

  /** Check that constraint is fully propagated. See comment in Config.checkConstraintsPropagated */
  def checkPropagated(msg: => String)(result: Boolean): Boolean = {
    if (Config.checkConstraintsPropagated && result && addConstraintInvocations == 0) {
      val saved = frozenConstraint
      frozenConstraint = true
      for (p <- constraint.domainParams) {
        def check(cond: => Boolean, q: PolyParam, ordering: String, explanation: String): Unit =
          assert(cond, i"propagation failure for $p $ordering $q: $explanation\n$msg")
        for (u <- constraint.upper(p))
          check(bounds(p).hi <:< bounds(u).hi, u, "<:", "upper bound not propagated")
        for (l <- constraint.lower(p)) {
          check(bounds(l).lo <:< bounds(p).hi, l, ">:", "lower bound not propagated")
          check(constraint.isLess(l, p), l, ">:", "reverse ordering (<:) missing")
        }
      }
      frozenConstraint = saved
    }
    result
  }
}
