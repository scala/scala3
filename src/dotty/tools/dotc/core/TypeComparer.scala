package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Flags._, Names._, NameOps._, Denotations._
import typer.Mode
import Decorators._
import StdNames.{nme, tpnme}
import collection.mutable
import printing.Disambiguation.disambiguated
import util.{Stats, DotClass, SimpleMap}
import config.Config
import config.Printers._
import TypeErasure.{erasedLub, erasedGlb}
import scala.util.control.NonFatal

/** Provides methods to compare types.
 */
class TypeComparer(initctx: Context) extends DotClass {
  implicit val ctx: Context = initctx

  val state = ctx.typerState
  import state.constraint

  private var pendingSubTypes: mutable.Set[(Type, Type)] = null
  private var recCount = 0

  /** If the constraint is frozen we cannot add new bounds to the constraint. */
  protected var frozenConstraint = false

  /** If the constraint is ignored, subtype checks only take into account
   *  declared bounds of PolyParams. Used when forming unions and intersectons
   *  of constraint bounds
   */
  protected var ignoreConstraint = false

  /** Compare a solution of the constraint instead of the constrained parameters.
   *  The solution maps every parameter to its lower bound.
   */
  protected var solvedConstraint = false

  private var needsGc = false

  /** Is a subtype check in course? In that case we may not
   *  permanently instantiate type variables, because the corresponding
   *  constraint might still be retracted and the instantiation should
   *  then be reversed.
   */
  def subtypeCheckInProgress: Boolean = {
    val result = recCount > 0
    if (result) {
      constr.println("*** needsGC ***")
      needsGc = true
    }
    result
  }

  /** For stastics: count how many isSubTypes are part of succesful comparisons */
  private var successCount = 0
  private var totalCount = 0

  private var myAnyClass: ClassSymbol = null
  private var myNothingClass: ClassSymbol = null
  private var myNullClass: ClassSymbol = null
  private var myObjectClass: ClassSymbol = null
  private var myAnyType: TypeRef = null

  def AnyClass = {
    if (myAnyClass == null) myAnyClass = defn.AnyClass
    myAnyClass
  }
  def NothingClass = {
    if (myNothingClass == null) myNothingClass = defn.NothingClass
    myNothingClass
  }
  def NullClass = {
    if (myNullClass == null) myNullClass = defn.NullClass
    myNullClass
  }
  def ObjectClass = {
    if (myObjectClass == null) myObjectClass = defn.ObjectClass
    myObjectClass
  }
  def AnyType = {
    if (myAnyType == null) myAnyType = AnyClass.typeRef
    myAnyType
  }

  // Constraint handling

  /** Map that approximates each param in constraint by its lower bound.
   *  Currently only used for diagnostics.
   */
  val approxParams = new TypeMap {
    def apply(tp: Type): Type = tp.stripTypeVar match {
      case tp: PolyParam if constraint contains tp =>
        this(constraint.bounds(tp).lo)
      case tp =>
        mapOver(tp)
    }
  }

  /** If `param` is contained in constraint, test whether its
   *  bounds are non-empty. Otherwise return `true`.
   */
  private def checkBounds(param: PolyParam): Boolean = constraint.at(param) match {
    case TypeBounds(lo, hi) =>
      if (Stats.monitored) Stats.record("checkBounds")
      isSubType(lo, hi)
    case _ => true
  }

  /** Test validity of constraint for parameter `changed` and of all
   *  parameters that depend on it.
   */
  private def propagate(changed: PolyParam): Boolean =
    if (Config.trackConstrDeps)
      checkBounds(changed) &&
      propagate(constraint.dependentParams(changed) - changed, Set(changed))
    else
      constraint forallParams checkBounds

  /** Ensure validity of constraints for parameters `params` and of all
   *  parameters that depend on them and that have not been tested
   *  now or before. If `trackConstrDeps` is not set, do this for all
   *  parameters in the constraint.
   *  @param  seen  the set of parameters that have been tested before.
   */
  private def propagate(params: Set[PolyParam], seen: Set[PolyParam]): Boolean =
    params.isEmpty ||
    (params forall checkBounds) && {
      val seen1 = seen ++ params
      val nextParams = (Set[PolyParam]() /: params) { (ps, p) =>
        ps ++ (constraint.dependentParams(p) -- seen1)
      }
      propagate(nextParams, seen1)
    }

  /** Check whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   *  As an optimization, when `trackConstrDeps` is set, we
   *  only test that the solutions satisfy the constraints `changed`
   *  and all parameters that depend on it.
   */
  def isSatisfiable(changed: PolyParam): Boolean = {
    val saved = solvedConstraint
    solvedConstraint = true
    try
      if (Config.trackConstrDeps) propagate(changed)
      else
        constraint.forallParams { param =>
          checkBounds(param) || {
            val TypeBounds(lo, hi) = constraint.bounds(param)
            ctx.log(i"sub fail $lo <:< $hi")
            ctx.log(i"approximated = ${approxParams(lo)} <:< ${approxParams(hi)}")
            false
          }
        }
    finally solvedConstraint = saved
  }

  /** Update constraint for `param` to `bounds`, check that
   *  new constraint is still satisfiable.
   */
  private def updateConstraint(param: PolyParam, bounds: TypeBounds): Boolean = {
    val saved = constraint
    constraint = constraint.updated(param, bounds)
    if (propagate(param)) {
      if (isSatisfiable(param)) return true
      ctx.log(i"SAT $constraint produced by $param $bounds is not satisfiable")
    }
    constraint = saved // don't leave the constraint in unsatisfiable state
    false
  }

  private def addConstraint1(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean = {
    val oldBounds = constraint.bounds(param)
    assert(!bound.isInstanceOf[TypeVar])
    val saved = ignoreConstraint
    ignoreConstraint = true
    val newBounds =
      try
        if (fromBelow) oldBounds.derivedTypeBounds(oldBounds.lo | bound, oldBounds.hi)
        else oldBounds.derivedTypeBounds(oldBounds.lo, oldBounds.hi & bound)
      finally ignoreConstraint = saved
    val res =
      (param == bound) || (oldBounds eq newBounds) || updateConstraint(param, newBounds)
    constr.println(s"added1 constraint $param ${if (fromBelow) ">:" else "<:"} $bound = $res")
    if (res) constr.println(constraint.show)
    res
  }

  /** Make p2 = p1, transfer all bounds of p2 to p1 */
  private def unify(p1: PolyParam, p2: PolyParam): Boolean = {
    constr.println(s"unifying $p1 $p2")
    val constraint1 = constraint.unify(p1, p2)
    val bounds = constraint1.bounds(p1)
    isSubType(bounds.lo, bounds.hi) && { constraint = constraint1; true }
  }

  /** If current constraint set is not frozen, add the constraint
   *
   *      param >: bound   if fromBelow is true
   *      param <: bound   otherwise
   *
   *  to the bounds of `param`. If `bound` is itself a constrained parameter, also
   *  add the dual constraint to `bound`.
   *  @pre `param` is in the constraint's domain
   *  @return Whether the augmented constraint is still satisfiable.
   */
  def addConstraint(param: PolyParam, bound0: Type, fromBelow: Boolean): Boolean = {
    assert(!frozenConstraint)
    val bound = bound0.dealias.stripTypeVar
    def description = s"${param.show} ${if (fromBelow) ">:>" else "<:<"} ${bound.show} (${bound.getClass}) to ${constraint.show}"
    constr.println(s"adding $description")
    val res = bound match {
      case bound: PolyParam if constraint contains bound =>
        val TypeBounds(lo, hi) = constraint.bounds(bound)
        if (lo eq hi)
          addConstraint(param, lo, fromBelow)
        else if (param == bound)
          true
        else if (fromBelow && param.occursIn(lo, fromBelow = true))
          unify(param, bound)
        else if (!fromBelow && param.occursIn(hi, fromBelow = false))
          unify(bound, param)
        else
          addConstraint1(param, bound, fromBelow) &&
          addConstraint1(bound, param, !fromBelow)
      case bound: AndOrType if fromBelow != bound.isAnd =>
        addConstraint(param, bound.tp1, fromBelow) &&
        addConstraint(param, bound.tp2, fromBelow)
      case bound: WildcardType =>
        true
      case bound => // !!! remove to keep the originals
        addConstraint1(param, bound, fromBelow)
    }
    constr.println(s"added $description = ${constraint.show}")
    res
  }

  def isConstrained(param: PolyParam): Boolean =
    !frozenConstraint && !solvedConstraint && (constraint contains param)

  /** Solve constraint set for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound. However, any occurrences
   *  of the parameter in a refinement somewhere in the bound are removed.
   *  (Such occurrences can arise for F-bounded types).
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  def approximation(param: PolyParam, fromBelow: Boolean): Type = {
    val avoidParam = new TypeMap {
      override def stopAtStatic = true
      def apply(tp: Type) = mapOver {
        tp match {
          case tp: RefinedType if param occursIn tp.refinedInfo => tp.parent
          case _ => tp
        }
      }
    }
    val bounds = constraint.bounds(param)
    val bound = if (fromBelow) bounds.lo else bounds.hi
    val inst = avoidParam(bound)
    typr.println(s"approx ${param.show}, from below = $fromBelow, bound = ${bound.show}, inst = ${inst.show}")
    inst
  }

  // Keeping track of seen refinements

  /** A map from refined names to the refined types in which they occur.
   *  During the subtype check involving the parent of a refined type,
   *  the refined name is stored in the map, so that the outermost
   *  refinements can be retrieved when interpreting a reference to the name.
   *  The name is associated with a pair of refinements. If the refinedInfo is
   *  skipped in sub- and super-type at the same time (first clause of
   *  `compareRefined`, both refinements are stored. If the name only appears
   *  as a refinement in the sub- or -super-type, the refinement type is stored
   *  twice as both elements of the pair.
   */
  protected var pendingRefinedBases: SimpleMap[Name, Set[(RefinedType, RefinedType)]]
    = SimpleMap.Empty

  /** Add pending name to `pendingRefinedBases`. */
  private def addPendingName(name: Name, rt1: RefinedType, rt2: RefinedType) = {
    var s = pendingRefinedBases(name)
    if (s == null) s = Set()
    pendingRefinedBases = pendingRefinedBases.updated(name, s + ((rt1, rt2)))
  }

  /** Given a selection of qualifier `qual` with given `name`, return a refined type
   *  that refines `qual`, or if that fails return `qual` itself.
   *  @param considerBoth  If true consider both lower and upper base of `name` when
   *                       checking for refinement (but always return the lower one)
   *  @see Type#refines
   */
  private def rebaseQual(qual: Type, name: Name, considerBoth: Boolean = false): Type = {
    val bases = pendingRefinedBases(name)
    if (bases == null) qual
    else bases.find {
      case (tp1, tp2) =>
        (tp1 refines qual) || considerBoth && (tp1 ne tp2) && (tp2 refines qual)
    } match {
      case Some((base1, _)) => base1
      case _ => qual
    }
  }

  private def narrowRefined(tp: Type): Type = tp match {
    case tp: RefinedType => RefinedThis(tp)
    case _ => tp
  }

  /** If the prefix of a named type is `this` (i.e. an instance of type
   *  `ThisType` or `RefinedThis`), and there is a refinement type R that
   *  "refines" (transitively contains as its parent) a class reference
   *  or refinement corresponding to the prefix, return the named type where
   *  the prefix is replaced by `RefinedThis(R)`. Otherwise return the named type itself.
   */
  private def rebase(tp: NamedType): Type = {
    def rebaseFrom(prefix: Type): Type = {
      rebaseQual(prefix, tp.name, considerBoth = true) match {
        case rt: RefinedType if rt ne prefix =>
          tp.derivedSelect(RefinedThis(rt)).dealias // dealias to short-circuit cycles spanning type aliases or LazyRefs
        case _ => tp
      }
    }
    tp.prefix match {
      case RefinedThis(rt) => rebaseFrom(rt)
      case pre: ThisType => rebaseFrom(pre.cls.info)
      case _ => tp
    }
  }

  /** If the given refined type is refined further, return the member
   *  of the refiend name relative to the refining base, otherwise return
   *  `refinedInfo`.
   *  TODO: Figure out why cannot simply write
   *
   *      rebaseQual(rt, rt.refinedName).member(rt.refinedName).info
   *
   *  (too much forcing, probably).
   */
  def normalizedInfo(rt: RefinedType) = {
    val base = rebaseQual(rt, rt.refinedName)
    if (base eq rt) rt.refinedInfo else base.member(rt.refinedName).info
  }

  // Subtype testing `<:<`

  def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
    if (tp2 eq NoType) return false
    if ((tp2 eq tp1) ||
        (tp2 eq WildcardType) ||
        (tp2 eq AnyType) && tp1.isValueType) return true
    isSubType(tp1, tp2)
  }

  def isSubTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    val saved = frozenConstraint
    frozenConstraint = true
    try isSubType(tp1, tp2)
    finally frozenConstraint = saved
  }

  def isNonBottomSubType(tp1: Type, tp2: Type): Boolean =
    !(tp2 isRef NothingClass) && isSubType(tp1, tp2)

  private def traceInfo(tp1: Type, tp2: Type) =
    s"${tp1.show} <:< ${tp2.show}" +
    (if (ctx.settings.verbose.value) s" ${tp1.getClass} ${tp2.getClass}${if (frozenConstraint) " frozen" else ""}" else "")

  def isSubType(tp1: Type, tp2: Type): Boolean = /*>|>*/ ctx.traceIndented(s"isSubType ${traceInfo(tp1, tp2)}", subtyping) /*<|<*/ {
    if (tp2 eq NoType) false
    else if (tp1 eq tp2) true
    else {
      val saved = constraint
      val savedSuccessCount = successCount
      try {
        recCount = recCount + 1
        val result =
          if (recCount < LogPendingSubTypesThreshold) firstTry(tp1, tp2)
          else monitoredIsSubType(tp1, tp2)
        recCount = recCount - 1
        if (!result) constraint = saved
        else if (recCount == 0 && needsGc) state.gc()

        def recordStatistics = {
          // Stats.record(s"isSubType ${tp1.show} <:< ${tp2.show}")
          totalCount += 1
          if (result) successCount += 1 else successCount = savedSuccessCount
          if (recCount == 0) {
            Stats.record("successful subType", successCount)
            Stats.record("total subType", totalCount)
            successCount = 0
            totalCount = 0
          }
        }
        if (Stats.monitored) recordStatistics

        result
      } catch {
        case NonFatal(ex) =>
          def showState =  {
            println(disambiguated(implicit ctx => s"assertion failure for ${tp1.show} <:< ${tp2.show}, frozen = $frozenConstraint"))
            def explainPoly(tp: Type) = tp match {
              case tp: PolyParam => println(s"polyparam ${tp.show} found in ${tp.binder.show}")
              case tp: TypeRef if tp.symbol.exists => println(s"typeref ${tp.show} found in ${tp.symbol.owner.show}")
              case tp: TypeVar => println(s"typevar ${tp.show}, origin = ${tp.origin}")
              case _ => println(s"${tp.show} is a ${tp.getClass}")
            }
            explainPoly(tp1)
            explainPoly(tp2)
          }
          if (ex.isInstanceOf[AssertionError]) showState
          recCount -= 1
          constraint = saved
          successCount = savedSuccessCount
          throw ex
      }
    }
  }

  def monitoredIsSubType(tp1: Type, tp2: Type) = {
    if (pendingSubTypes == null) {
      pendingSubTypes = new mutable.HashSet[(Type, Type)]
      ctx.log(s"!!! deep subtype recursion involving ${tp1.show} <:< ${tp2.show}, constraint = ${state.constraint.show}")
      ctx.log(s"!!! constraint = ${constraint.show}")
      assert(!ctx.settings.YnoDeepSubtypes.value)
      if (Config.traceDeepSubTypeRecursions && !this.isInstanceOf[ExplainingTypeComparer])
        ctx.log(TypeComparer.explained(implicit ctx => ctx.typeComparer.isSubType(tp1, tp2)))
    }
    val p = (tp1, tp2)
    !pendingSubTypes(p) && {
      try {
        pendingSubTypes += p
        firstTry(tp1, tp2)
      } finally {
        pendingSubTypes -= p
      }
    }
  }

  def firstTry(tp1: Type, tp2: Type): Boolean = {
    tp2 match {
      case tp2: NamedType =>
        def isHKSubType = tp2.name == tpnme.Apply && {
          val lambda2 = tp2.prefix.LambdaClass(forcing = true)
          lambda2.exists && !tp1.isLambda &&
            tp1.testLifted(lambda2.typeParams, isSubType(_, tp2.prefix))
        }
        def compareNamed = {
          implicit val ctx: Context = this.ctx // Dotty deviation: implicits need explicit type
          tp1 match {
            case tp1: NamedType =>
              val sym1 = tp1.symbol
              (if ((sym1 ne NoSymbol) && (sym1 eq tp2.symbol)) (
                   ctx.erasedTypes
                || sym1.isStaticOwner
                || { // Implements: A # X  <:  B # X
                     // if either A =:= B (i.e. A <: B and B <: A), or the following three conditions hold:
                     //  1. X is a class type,
                     //  2. B is a class type without abstract type members.
                     //  3. A <: B.
                     // Dealiasing is taken care of elsewhere.
                     val pre1 = tp1.prefix
                     val pre2 = tp2.prefix
                     (  isSameType(pre1, pre2)
                     ||    sym1.isClass
                        && pre2.classSymbol.exists
                        && pre2.abstractTypeMembers.isEmpty
                        && isSubType(pre1, pre2)
                     )
                   }
                )
              else (tp1.prefix ne tp2.prefix) && (tp1.name eq tp2.name)
                && isSameType(tp1.prefix, tp2.prefix)
             ) || isHKSubType || secondTryNamed(tp1, tp2)
            case tp1: ThisType if tp1.cls eq tp2.symbol.moduleClass =>
              isSubType(tp1.cls.owner.thisType, tp2.prefix)
            case _ =>
              isHKSubType || secondTry(tp1, tp2)
          }
        }
        compareNamed
      case tp2: ProtoType =>
        isMatchedByProto(tp2, tp1)
      case tp2: PolyParam =>
        def comparePolyParam =
          tp2 == tp1 || {
            if (solvedConstraint && (constraint contains tp2)) isSubType(tp1, bounds(tp2).lo)
            else
              isSubTypeWhenFrozen(tp1, bounds(tp2).lo) || {
                if (isConstrained(tp2)) addConstraint(tp2, tp1.widenExpr, fromBelow = true)
                else (ctx.mode is Mode.TypevarsMissContext) || secondTry(tp1, tp2)
              }
          }
        comparePolyParam
      case tp2: BoundType =>
        tp2 == tp1 || secondTry(tp1, tp2)
      case tp2: TypeVar =>
        isSubType(tp1, tp2.underlying)
      case tp2: WildcardType =>
        def compareWild = tp2.optBounds match {
          case TypeBounds(_, hi) => isSubType(tp1, hi)
          case NoType => true
        }
        compareWild
      case tp2: LazyRef =>
        isSubType(tp1, tp2.ref)
      case tp2: AnnotatedType =>
        isSubType(tp1, tp2.tpe) // todo: refine?
      case tp2: ThisType =>
        tp1 match {
          case tp1: ThisType =>
            // We treat two prefixes A.this, B.this as equivalent if
            // A's selftype derives from B and B's selftype derives from A.
            tp1.cls.classInfo.selfType.derivesFrom(tp2.cls) &&
            tp2.cls.classInfo.selfType.derivesFrom(tp1.cls)
          case _ =>
            secondTry(tp1, tp2)
        }
      case tp2: SuperType =>
        tp1 match {
          case tp1: SuperType =>
            isSubType(tp1.thistpe, tp2.thistpe) &&
            isSameType(tp1.supertpe, tp2.supertpe)
          case _ =>
            secondTry(tp1, tp2)
        }
      case AndType(tp21, tp22) =>
        isSubType(tp1, tp21) && isSubType(tp1, tp22)
      case ErrorType =>
        true
      case _ =>
        secondTry(tp1, tp2)
    }
  }

  def secondTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: NamedType =>
      tp2 match {
        case tp2: ThisType if tp2.cls eq tp1.symbol.moduleClass =>
          isSubType(tp1.prefix, tp2.cls.owner.thisType)
        case _ =>
          secondTryNamed(tp1, tp2)
      }
    case OrType(tp11, tp12) =>
      isSubType(tp11, tp2) && isSubType(tp12, tp2)
    case tp1: PolyParam =>
      def comparePolyParam =
        tp1 == tp2 || {
          if (solvedConstraint && (constraint contains tp1)) isSubType(bounds(tp1).lo, tp2)
          else
            isSubTypeWhenFrozen(bounds(tp1).hi, tp2) || {
              if (isConstrained(tp1))
                addConstraint(tp1, tp2, fromBelow = false) && {
                  if ((!frozenConstraint) &&
                    (tp2 isRef defn.NothingClass) &&
                    state.isGlobalCommittable) {
                    def msg = s"!!! instantiated to Nothing: $tp1, constraint = ${constraint.show}"
                    if (Config.flagInstantiationToNothing) assert(false, msg)
                    else ctx.log(msg)
                  }
                  true
                }
              else (ctx.mode is Mode.TypevarsMissContext) || thirdTry(tp1, tp2)
            }
        }
      comparePolyParam
    case tp1: RefinedThis =>
      tp2 match {
        case tp2: RefinedThis if tp1.binder.parent =:= tp2.binder.parent => true
        case _ => thirdTry(tp1, tp2)
      }
    case tp1: BoundType =>
      tp1 == tp2 || thirdTry(tp1, tp2)
    case tp1: TypeVar =>
      (tp1 eq tp2) || isSubType(tp1.underlying, tp2)
    case tp1: WildcardType =>
      def compareWild = tp1.optBounds match {
        case TypeBounds(lo, _) => isSubType(lo, tp2)
        case _ => true
      }
      compareWild
    case tp1: LazyRef =>
      isSubType(tp1.ref, tp2)
    case tp1: AnnotatedType =>
      isSubType(tp1.tpe, tp2)
    case ErrorType =>
      true
    case _ =>
      thirdTry(tp1, tp2)
  }

  def secondTryNamed(tp1: NamedType, tp2: Type): Boolean = {
    def tryRebase2nd = {
      val tp1rebased = rebase(tp1)
      if (tp1rebased ne tp1) isSubType(tp1rebased, tp2)
      else thirdTry(tp1, tp2)
    }
    tp1.info match {
      // There was the following code, which was meant to implement this logic:
      //    If x has type A | B, then x.type <: C if
      //    x.type <: C assuming x has type A, and
      //    x.type <: C assuming x has type B.
      // But it did not work, because derivedRef would always give back the same
      // type and cache the denotation. So it ended up copmparing just one branch.
      // The code seems to be unncessary for the tests and does not seems to help performance.
      // So it is commented out. If we ever need to come back to this, we would have
      // to create unchached TermRefs in order to avoid cross talk between the branches.
      /*
      case OrType(tp11, tp12) =>
        val sd = tp1.denot.asSingleDenotation
        def derivedRef(tp: Type) =
          NamedType(tp1.prefix, tp1.name, sd.derivedSingleDenotation(sd.symbol, tp))
        secondTry(OrType.make(derivedRef(tp11), derivedRef(tp12)), tp2)
      */
      case TypeBounds(lo1, hi1) =>
        val gbounds1 = ctx.gadt.bounds(tp1.symbol)
        if (gbounds1 != null)
          isSubTypeWhenFrozen(gbounds1.hi, tp2) ||
            (ctx.mode is Mode.GADTflexible) &&
             narrowGADTBounds(tp1, TypeBounds(gbounds1.lo, gbounds1.hi & tp2)) ||
             tryRebase2nd
        else if (lo1 eq hi1) isSubType(hi1, tp2)
        else tryRebase2nd
      case _ =>
        tryRebase2nd
    }
  }

  def thirdTry(tp1: Type, tp2: Type): Boolean = tp2 match {
    case tp2: NamedType =>
      def tryRebase3rd = {
        val tp2rebased = rebase(tp2)
        if (tp2rebased ne tp2) isSubType(tp1, tp2rebased)
        else fourthTry(tp1, tp2)
      }
      def compareNamed: Boolean = tp2.info match {
        case TypeBounds(lo2, hi2) =>
          val gbounds2 = ctx.gadt.bounds(tp2.symbol)
          if (gbounds2 != null)
            isSubTypeWhenFrozen(tp1, gbounds2.lo) ||
              (ctx.mode is Mode.GADTflexible) &&
              narrowGADTBounds(tp2, TypeBounds(gbounds2.lo | tp1, gbounds2.hi)) ||
             tryRebase3rd
          else
            ((frozenConstraint || !isCappable(tp1)) && isSubType(tp1, lo2)
            || tryRebase3rd)

        case _ =>
          val cls2 = tp2.symbol
          if (cls2.isClass) {
            val base = tp1.baseTypeRef(cls2)
            if (base.exists && (base ne tp1)) return isSubType(base, tp2)
            if (cls2 == defn.SingletonClass && tp1.isStable) return true
          }
          tryRebase3rd
      }
      compareNamed
    case tp2 @ RefinedType(parent2, name2) =>
        def qualifies(m: SingleDenotation) = isSubType(m.info, tp2.refinedInfo)
        def memberMatches(mbr: Denotation): Boolean = mbr match { // inlined hasAltWith for performance
          case mbr: SingleDenotation => qualifies(mbr)
          case _ => mbr hasAltWith qualifies
        }
      def compareRefinedSlow: Boolean = {
        def hasMatchingMember(name: Name): Boolean = /*>|>*/ ctx.traceIndented(s"hasMatchingMember($name) ${tp1.member(name).info.show}", subtyping) /*<|<*/ {
          val tp1r = rebaseQual(tp1, name)
          (memberMatches(narrowRefined(tp1r) member name)
            ||
            { // special case for situations like:
              //    foo <: C { type T = foo.T }
              tp2.refinedInfo match {
                case rinfo: TypeAlias =>
                  !ctx.phase.erasedTypes && (tp1r select name) =:= rinfo.alias
                case _ => false
              }
            })
        }
        val matchesParent = {
          val saved = pendingRefinedBases
          try {
            addPendingName(name2, tp2, tp2)
            isSubType(tp1, parent2)
          } finally pendingRefinedBases = saved
        }
        (matchesParent && (
          name2 == nme.WILDCARD
          || hasMatchingMember(name2)
          || fourthTry(tp1, tp2))
          || needsEtaLift(tp1, tp2) && tp1.testLifted(tp2.typeParams, isSubType(_, tp2)))
      }
      def compareRefined: Boolean = tp1.widen match {
        case tp1 @ RefinedType(parent1, name1) if name1 == name2 && name1.isTypeName =>
          normalizedInfo(tp1) match {
            case bounds1 @ TypeBounds(lo1, hi1) if lo1 eq hi1 =>
              isSubType(bounds1, tp2.refinedInfo) && {
                val saved = pendingRefinedBases
                try {
                  addPendingName(name1, tp1, tp2)
                  isSubType(parent1, parent2)
                } finally pendingRefinedBases = saved
              }
            case _ =>
              compareRefinedSlow
          }
        case _ =>
          compareRefinedSlow
      }
      compareRefined
    case OrType(tp21, tp22) =>
      eitherIsSubType(tp1, tp21, tp1, tp22) || fourthTry(tp1, tp2)
    case tp2 @ MethodType(_, formals2) =>
      def compareMethod = tp1 match {
        case tp1 @ MethodType(_, formals1) =>
          (tp1.signature sameParams tp2.signature) &&
            (if (Config.newMatch) subsumeParams(formals1, formals2, tp1.isJava, tp2.isJava)
             else matchingParams(formals1, formals2, tp1.isJava, tp2.isJava)) &&
            tp1.isImplicit == tp2.isImplicit && // needed?
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
      compareMethod
    case tp2: PolyType =>
      def comparePoly = tp1 match {
        case tp1: PolyType =>
          (tp1.signature sameParams tp2.signature) &&
          matchingTypeParams(tp1, tp2) &&
          isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
      comparePoly
    case tp2 @ ExprType(restpe2) =>
      def compareExpr = tp1 match {
        // We allow ()T to be a subtype of => T.
        // We need some subtype relationship between them so that e.g.
        // def toString   and   def toString()   don't clash when seen
        // as members of the same type. And it seems most logical to take
        // ()T <:< => T, since everything one can do with a => T one can
        // also do with a ()T by automatic () insertion.
        case tp1 @ MethodType(Nil, _) => isSubType(tp1.resultType, restpe2)
        case _ => isSubType(tp1.widenExpr, restpe2)
      }
      compareExpr
    case tp2 @ TypeBounds(lo2, hi2) =>
      def compareTypeBounds = tp1 match {
        case tp1 @ TypeBounds(lo1, hi1) =>
          (tp2.variance > 0 && tp1.variance >= 0 || isSubType(lo2, lo1)) &&
          (tp2.variance < 0 && tp1.variance <= 0 || isSubType(hi1, hi2))
        case tp1: ClassInfo =>
          val tt = tp1.typeRef
          isSubType(lo2, tt) && isSubType(tt, hi2)
        case _ =>
          false
      }
      compareTypeBounds
    case ClassInfo(pre2, cls2, _, _, _) =>
      def compareClassInfo = tp1 match {
        case ClassInfo(pre1, cls1, _, _, _) =>
          (cls1 eq cls2) && isSubType(pre2, pre1)
        case _ =>
          false
      }
      compareClassInfo
    case JavaArrayType(elem2) =>
      def compareJavaArray = tp1 match {
        case JavaArrayType(elem1) => isSubType(elem1, elem2)
        case _ => fourthTry(tp1, tp2)
      }
      compareJavaArray
    case _ =>
      fourthTry(tp1, tp2)
  }

  def fourthTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: TypeRef =>
      tp1.info match {
        case TypeBounds(lo1, hi1) =>
          isSubType(hi1, tp2)
        case _ =>
          def isNullable(tp: Type): Boolean = tp.dealias match {
            case tp: TypeRef => tp.symbol.isNullableClass
            case RefinedType(parent, _) => isNullable(parent)
            case _ => false
          }
          (tp1.symbol eq NothingClass) && tp2.isInstanceOf[ValueType] ||
          (tp1.symbol eq NullClass) && isNullable(tp2)
      }
    case tp1: SingletonType =>
      isNewSubType(tp1.underlying.widenExpr, tp2) || {
        // if tp2 == p.type  and p: q.type then try   tp1 <:< q.type as a last effort.
        tp2 match {
          case tp2: TermRef =>
            tp2.info match {
              case tp2i: TermRef =>
                isSubType(tp1, tp2i)
              case ExprType(tp2i: TermRef) if (ctx.phase.id > ctx.gettersPhase.id) =>
                isSubType(tp1, tp2i)
              case _ =>
                false
            }
          case _ =>
            false
        }
      }
    case tp1: RefinedType =>
      { val saved = pendingRefinedBases
        try {
          addPendingName(tp1.refinedName, tp1, tp1)
          isNewSubType(tp1.parent, tp2)
        }
        finally pendingRefinedBases = saved
      } || needsEtaLift(tp2, tp1) && tp2.testLifted(tp1.typeParams, isSubType(tp1, _))
    case AndType(tp11, tp12) =>
      eitherIsSubType(tp11, tp2, tp12, tp2)
    case JavaArrayType(elem1) =>
      tp2 isRef ObjectClass
    case _ =>
      false
  }

  /** Returns true iff either `tp11 <:< tp21` or `tp12 <:< tp22`, trying at the same time
   *  to keep the constraint as wide as possible. Specifically, if
   *
   *    tp11 <:< tp12 = true   with post-constraint c1
   *    tp12 <:< tp22 = true   with post-constraint c2
   *
   *  and c1 subsumes c2, then c2 is kept as the post-constraint of the result,
   *  otherwise c1 is kept.
   *
   *  This method is used to approximate a solution in one of the following cases
   *
   *     T1 & T2 <:< T3
   *     T1 <:< T2 | T3
   *
   *  In the first case (the second one is analogous), we have a choice whether we
   *  want to establish the subtyping judgement using
   *
   *     T1 <:< T3   or    T2 <:< T3
   *
   *  as a precondition. Either precondition might constrain type variables.
   *  The purpose of this method is to pick the precondition that constrains less.
   *  The method is not complete, because sometimes there is no best solution. Example:
   *
   *     A? & B?  <:  T
   *
   *  Here, each precondition leads to a different constraint, and neither of
   *  the two post-constraints subsumes the other.
   */
  def eitherIsSubType(tp11: Type, tp21: Type, tp12: Type, tp22: Type) = {
    val preConstraint = constraint
    isSubType(tp11, tp21) && {
      val leftConstraint = constraint
      constraint = preConstraint
      if (isSubType(tp12, tp22) && !subsumes(leftConstraint, constraint, preConstraint))
        constraint = leftConstraint
      true
    } || isSubType(tp12, tp22)
  }

  /** Like tp1 <:< tp2, but returns false immediately if we know that
   *  the case was covered previously during subtyping.
   */
  private def isNewSubType(tp1: Type, tp2: Type): Boolean =
    if (isCovered(tp1) && isCovered(tp2)) {
      //println(s"useless subtype: $tp1 <:< $tp2")
      false
    }
    else isSubType(tp1, tp2)

  /** A type has been covered previously in subtype checking if it
   *  is some combination of TypeRefs that point to classes, where the
   *  combiners are RefinedTypes, AndTypes or AnnotatedTypes.
   */
  private def isCovered(tp: Type): Boolean = tp.dealias.stripTypeVar match {
    case tp: TypeRef => tp.symbol.isClass && tp.symbol != NothingClass && tp.symbol != NullClass
    case tp: ProtoType => false
    case tp: RefinedType => isCovered(tp.parent)
    case tp: AnnotatedType => isCovered(tp.underlying)
    case AndType(tp1, tp2) => isCovered(tp1) && isCovered(tp2)
    case _ => false
  }

  /** The current bounds of type parameter `param` */
  def bounds(param: PolyParam): TypeBounds = constraint at param match {
    case bounds: TypeBounds if !ignoreConstraint => bounds
    case _ => param.binder.paramBounds(param.paramNum)
  }

  /** Defer constraining type variables when compared against prototypes */
  def isMatchedByProto(proto: ProtoType, tp: Type) = tp.stripTypeVar match {
    case tp: PolyParam if !solvedConstraint && (constraint contains tp) => true
    case _ => proto.isMatchedBy(tp)
  }

  /** Can type `tp` be constrained from above by adding a constraint to
   *  a typevar that it refers to? In that case we have to be careful not
   *  to approximate with the lower bound of a type in `thirdTry`. Instead,
   *  we should first unroll `tp1` until we hit the type variable and bind the
   *  type variable with (the corresponding type in) `tp2` instead.
   */
  def isCappable(tp: Type): Boolean = tp match {
    case tp: PolyParam => !solvedConstraint && (constraint contains tp)
    case tp: TypeProxy => isCappable(tp.underlying)
    case tp: AndOrType => isCappable(tp.tp1) || isCappable(tp.tp2)
    case _ => false
  }

  /** Does `tp` need to be eta lifted to be comparable to `target`? */
  def needsEtaLift(tp: Type, target: RefinedType) = {
    val name = target.refinedName
    (name.isLambdaArgName || (name eq tpnme.Apply)) && target.isLambda &&
    tp.exists && !tp.isLambda
  }

  def narrowGADTBounds(tr: NamedType, bounds: TypeBounds): Boolean =
    isSubType(bounds.lo, bounds.hi) &&
    { ctx.gadt.setBounds(tr.symbol, bounds); true }

  // Tests around `matches`

  /** A function implementing `tp1` matches `tp2`. */
  final def matchesType(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = tp1 match {
    case tp1: MethodType =>
      tp2 match {
        case tp2: MethodType =>
          tp1.isImplicit == tp2.isImplicit &&
            matchingParams(tp1.paramTypes, tp2.paramTypes, tp1.isJava, tp2.isJava) &&
            matchesType(tp1.resultType, tp2.resultType.subst(tp2, tp1), alwaysMatchSimple)
        case tp2: ExprType =>
          tp1.paramNames.isEmpty &&
            matchesType(tp1.resultType, tp2.resultType, alwaysMatchSimple)
        case _ =>
          false
      }
    case tp1: ExprType =>
      tp2 match {
        case tp2: MethodType =>
          tp2.paramNames.isEmpty &&
            matchesType(tp1.resultType, tp2.resultType, alwaysMatchSimple)
        case tp2: ExprType =>
          matchesType(tp1.resultType, tp2.resultType, alwaysMatchSimple)
        case _ =>
          false // was: matchesType(tp1.resultType, tp2, alwaysMatchSimple)
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType =>
          sameLength(tp1.paramNames, tp2.paramNames) &&
            matchesType(tp1.resultType, tp2.resultType.subst(tp2, tp1), alwaysMatchSimple)
        case _ =>
          false
      }
    case _ =>
      tp2 match {
        case _: MethodType | _: PolyType =>
          false
        case tp2: ExprType =>
          false // was: matchesType(tp1, tp2.resultType, alwaysMatchSimple)
        case _ =>
          alwaysMatchSimple || isSameType(tp1, tp2)
      }
  }

  /** Are `syms1` and `syms2` parameter lists with pairwise equivalent types? */
  private def matchingParams(formals1: List[Type], formals2: List[Type], isJava1: Boolean, isJava2: Boolean): Boolean = formals1 match {
    case formal1 :: rest1 =>
      formals2 match {
        case formal2 :: rest2 =>
          (isSameType(formal1, formal2)
            || isJava1 && (formal2 isRef ObjectClass) && (formal1 isRef AnyClass)
            || isJava2 && (formal1 isRef ObjectClass) && (formal2 isRef AnyClass)) &&
          matchingParams(rest1, rest2, isJava1, isJava2)
        case nil =>
          false
      }
    case nil =>
      formals2.isEmpty
  }

  private def subsumeParams(formals1: List[Type], formals2: List[Type], isJava1: Boolean, isJava2: Boolean): Boolean = formals1 match {
    case formal1 :: rest1 =>
      formals2 match {
        case formal2 :: rest2 =>
          (isSubType(formal2, formal1)
            || isJava1 && (formal2 isRef ObjectClass) && (formal1 isRef AnyClass)
            || isJava2 && (formal1 isRef ObjectClass) && (formal2 isRef AnyClass)) &&
          subsumeParams(rest1, rest2, isJava1, isJava2)
        case nil =>
          false
      }
    case nil =>
      formals2.isEmpty
  }

  /** Do poly types `poly1` and `poly2` have type parameters that
   *  have the same bounds (after renaming one set to the other)?
   */
  private def matchingTypeParams(poly1: PolyType, poly2: PolyType): Boolean =
    (poly1.paramBounds corresponds poly2.paramBounds)((b1, b2) =>
      isSameType(b1, b2.subst(poly2, poly1)))

  // Type equality =:=

  /** Two types are the same if are mutual subtypes of each other */
  def isSameType(tp1: Type, tp2: Type): Boolean =
    if (tp1 eq NoType) false
    else if (tp1 eq tp2) true
    else isSubType(tp1, tp2) && isSubType(tp2, tp1)

  /** Same as `isSameType` but also can be applied to overloaded TermRefs, where
   *  two overloaded refs are the same if they have pairwise equal alternatives
   */
  def isSameRef(tp1: Type, tp2: Type): Boolean = ctx.traceIndented(s"isSameRef($tp1, $tp2") {
    def isSubRef(tp1: Type, tp2: Type): Boolean = tp1 match {
      case tp1: TermRef if tp1.isOverloaded =>
        tp1.alternatives forall (isSubRef(_, tp2))
      case _ =>
        tp2 match {
          case tp2: TermRef if tp2.isOverloaded =>
            tp2.alternatives exists (isSubRef(tp1, _))
          case _ =>
            isSubType(tp1, tp2)
        }
    }
    isSubRef(tp1, tp2) && isSubRef(tp2, tp1)
  }

  /** The greatest lower bound of two types */
  def glb(tp1: Type, tp2: Type): Type = /*>|>*/ ctx.traceIndented(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true) /*<|<*/ {
    if (tp1 eq tp2) tp1
    else if (!tp1.exists) tp2
    else if (!tp2.exists) tp1
    else if ((tp1 isRef AnyClass) || (tp2 isRef NothingClass)) tp2
    else if ((tp2 isRef AnyClass) || (tp1 isRef NothingClass)) tp1
    else tp2 match {  // normalize to disjunctive normal form if possible.
      case OrType(tp21, tp22) =>
        tp1 & tp21 | tp1 & tp22
      case _ =>
        tp1 match {
          case OrType(tp11, tp12) =>
            tp11 & tp2 | tp12 & tp2
          case _ =>
            val t1 = mergeIfSub(tp1, tp2)
            if (t1.exists) t1
            else {
              val t2 = mergeIfSub(tp2, tp1)
              if (t2.exists) t2
              else andType(tp1, tp2)
            }
        }
    }
  }

  /** The greatest lower bound of a list types */
  final def glb(tps: List[Type]): Type =
    (defn.AnyType /: tps)(glb)

  /** The least upper bound of two types
   *  @note  We do not admit singleton types in or-types as lubs.
   */
  def lub(tp1: Type, tp2: Type): Type = /*>|>*/ ctx.traceIndented(s"lub(${tp1.show}, ${tp2.show})", subtyping, show = true) /*<|<*/ {
    if (tp1 eq tp2) tp1
    else if (!tp1.exists) tp1
    else if (!tp2.exists) tp2
    else if ((tp1 isRef AnyClass) || (tp2 isRef NothingClass)) tp1
    else if ((tp2 isRef AnyClass) || (tp1 isRef NothingClass)) tp2
    else {
      val t1 = mergeIfSuper(tp1, tp2)
      if (t1.exists) t1
      else {
        val t2 = mergeIfSuper(tp2, tp1)
        if (t2.exists) t2
        else {
          val tp1w = tp1.widen
          val tp2w = tp2.widen
          if ((tp1 ne tp1w) || (tp2 ne tp2w)) lub(tp1w, tp2w)
          else orType(tp1w, tp2w) // no need to check subtypes again
        }
      }
    }
  }

  /** The least upper bound of a list of types */
  final def lub(tps: List[Type]): Type =
    (defn.NothingType /: tps)(lub)

  /** Merge `t1` into `tp2` if t1 is a subtype of some &-summand of tp2.
   */
  private def mergeIfSub(tp1: Type, tp2: Type): Type =
    if (isSubTypeWhenFrozen(tp1, tp2))
      if (isSubTypeWhenFrozen(tp2, tp1)) tp2 else tp1 // keep existing type if possible
    else tp2 match {
      case tp2 @ AndType(tp21, tp22) =>
        val lower1 = mergeIfSub(tp1, tp21)
        if (lower1 eq tp21) tp2
        else if (lower1.exists) lower1 & tp22
        else {
          val lower2 = mergeIfSub(tp1, tp22)
          if (lower2 eq tp22) tp2
          else if (lower2.exists) tp21 & lower2
          else NoType
        }
      case _ =>
        NoType
    }

  /** Merge `tp1` into `tp2` if tp1 is a supertype of some |-summand of tp2.
   */
  private def mergeIfSuper(tp1: Type, tp2: Type): Type =
    if (isSubTypeWhenFrozen(tp2, tp1))
      if (isSubTypeWhenFrozen(tp1, tp2)) tp2 else tp1 // keep existing type if possible
    else tp2 match {
      case tp2 @ OrType(tp21, tp22) =>
        val higher1 = mergeIfSuper(tp1, tp21)
        if (higher1 eq tp21) tp2
        else if (higher1.exists) higher1 | tp22
        else {
          val higher2 = mergeIfSuper(tp1, tp22)
          if (higher2 eq tp22) tp2
          else if (higher2.exists) tp21 | higher2
          else NoType
        }
      case _ =>
        NoType
    }

  /** Form a normalized conjunction of two types.
   *  Note: For certain types, `&` is distributed inside the type. This holds for
   *  all types which are not value types (e.g. TypeBounds, ClassInfo,
   *  ExprType, MethodType, PolyType). Also, when forming an `&`,
   *  instantiated TypeVars are dereferenced and annotations are stripped.
   *  Finally, refined types with the same refined name are
   *  opportunistically merged.
   *
   *  Sometimes, the conjunction of two types cannot be formed because
   *  the types are in conflict of each other. In particular:
   *
   *    1. Two different class types are conflicting.
   *    2. A class type conflicts with a type bounds that does not include the class reference.
   *    3. Two method or poly types with different (type) parameters but the same
   *       signature are conflicting
   *
   *  In these cases, one of the types is picked (@see andConflict).
   *  This is arbitrary, but I believe it is analogous to forming
   *  infeasible TypeBounds (where low bound is not a subtype of high bound).
   *  Such TypeBounds can also be arbitrarily instantiated. In both cases we need to
   *  make sure that such types do not actually arise in source programs.
   */
  final def andType(tp1: Type, tp2: Type, erased: Boolean = ctx.erasedTypes) = ctx.traceIndented(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true) {
    val t1 = distributeAnd(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeAnd(tp2, tp1)
      if (t2.exists) t2
      else if (erased) erasedGlb(tp1, tp2, isJava = false)
      else {
        //if (isHKRef(tp1)) tp2
        //else if (isHKRef(tp2)) tp1
        //else
        AndType(tp1, tp2)
      }
    }
  }

  /** Form a normalized conjunction of two types.
   *  Note: For certain types, `|` is distributed inside the type. This holds for
   *  all types which are not value types (e.g. TypeBounds, ClassInfo,
   *  ExprType, MethodType, PolyType). Also, when forming an `|`,
   *  instantiated TypeVars are dereferenced and annotations are stripped.
   *
   *  Sometimes, the disjunction of two types cannot be formed because
   *  the types are in conflict of each other. (@see `andType` for an enumeration
   *  of these cases). In cases of conflict a `MergeError` is raised.
   *
   *  @param erased   Apply erasure semantics. If erased is true, instead of creating
   *                  an OrType, the lub will be computed using TypeCreator#erasedLub.
   */
  final def orType(tp1: Type, tp2: Type, erased: Boolean = ctx.erasedTypes) = {
    val t1 = distributeOr(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeOr(tp2, tp1)
      if (t2.exists) t2
      else if (erased) erasedLub(tp1, tp2)
      else
        //if (isHKRef(tp1)) tp1
        //else if (isHKRef(tp2)) tp2
        //else
        OrType(tp1, tp2)
    }
  }

  /** Try to distribute `&` inside type, detect and handle conflicts */
  private def distributeAnd(tp1: Type, tp2: Type): Type = tp1 match {
    // opportunistically merge same-named refinements
    // this does not change anything semantically (i.e. merging or not merging
    // gives =:= types), but it keeps the type smaller.
    case tp1: RefinedType =>
      tp2 match {
        case tp2: RefinedType if tp1.refinedName == tp2.refinedName =>
          tp1.derivedRefinedType(
              tp1.parent & tp2.parent,
              tp1.refinedName,
              tp1.refinedInfo & tp2.refinedInfo)
        case _ =>
          NoType
      }
    case tp1: TypeBounds =>
      tp2 match {
        case tp2: TypeBounds => tp1 & tp2
        case _ => andConflict(tp1, tp2)
      }
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo if tp1.cls eq tp2.cls =>
          tp1.derivedClassInfo(tp1.prefix & tp2.prefix)
        case _ =>
          andConflict(tp1, tp2)
      }
    case tp1 @ MethodType(names1, formals1) =>
      tp2 match {
        case tp2 @ MethodType(names2, formals2)
        if Config.newMatch && (tp1.isImplicit == tp2.isImplicit) && formals1.hasSameLengthAs(formals2) =>
          tp1.derivedMethodType(
              mergeNames(names1, names2, nme.syntheticParamName),
              (formals1 zipWithConserve formals2)(_ | _),
              tp1.resultType & tp2.resultType.subst(tp2, tp1))
        case tp2 @ MethodType(names2, formals2)
        if matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
           tp1.isImplicit == tp2.isImplicit =>
          tp1.derivedMethodType(
              mergeNames(names1, names2, nme.syntheticParamName),
              formals1, tp1.resultType & tp2.resultType.subst(tp2, tp1))
        case _ =>
          andConflict(tp1, tp2)
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType if matchingTypeParams(tp1, tp2) =>
          tp1.derivedPolyType(
              mergeNames(tp1.paramNames, tp2.paramNames, tpnme.syntheticTypeParamName),
              tp1.paramBounds, tp1.resultType & tp2.resultType.subst(tp2, tp1))
        case _ =>
          andConflict(tp1, tp2)
      }
    case ExprType(rt1) =>
      tp2 match {
        case ExprType(rt2) =>
          ExprType(rt1 & rt2)
        case _ =>
          rt1 & tp2
      }
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying & tp2
    case tp1: AnnotatedType =>
      tp1.underlying & tp2
    case _ =>
      NoType
  }

  /** Try to distribute `|` inside type, detect and handle conflicts */
  private def distributeOr(tp1: Type, tp2: Type): Type = tp1 match {
    case tp1: RefinedType =>
      tp2 match {
        case tp2: RefinedType if tp1.refinedName == tp2.refinedName =>
          tp1.derivedRefinedType(
              tp1.parent | tp2.parent,
              tp1.refinedName,
              tp1.refinedInfo | tp2.refinedInfo)
        case _ =>
          NoType
      }
    case tp1: TypeBounds =>
      tp2 match {
        case tp2: TypeBounds => tp1 | tp2
        case _ => orConflict(tp1, tp2)
      }
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo if tp1.cls eq tp2.cls =>
          tp1.derivedClassInfo(tp1.prefix | tp2.prefix)
        case _ =>
          orConflict(tp1, tp2)
      }
    case tp1 @ MethodType(names1, formals1) =>
      tp2 match {
        case tp2 @ MethodType(names2, formals2)
        if Config.newMatch && (tp1.isImplicit == tp2.isImplicit) && formals1.hasSameLengthAs(formals2) =>
          tp1.derivedMethodType(
              mergeNames(names1, names2, nme.syntheticParamName),
              (formals1 zipWithConserve formals2)(_ & _),
              tp1.resultType | tp2.resultType.subst(tp2, tp1))
        case tp2 @ MethodType(names2, formals2)
        if matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
           tp1.isImplicit == tp2.isImplicit =>
          tp1.derivedMethodType(
              mergeNames(names1, names2, nme.syntheticParamName),
              formals1, tp1.resultType | tp2.resultType.subst(tp2, tp1))
        case _ =>
          orConflict(tp1, tp2)
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType if matchingTypeParams(tp1, tp2) =>
          tp1.derivedPolyType(
              mergeNames(tp1.paramNames, tp2.paramNames, tpnme.syntheticTypeParamName),
              tp1.paramBounds, tp1.resultType | tp2.resultType.subst(tp2, tp1))
        case _ =>
          orConflict(tp1, tp2)
      }
    case ExprType(rt1) =>
      ExprType(rt1 | tp2.widenExpr)
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying | tp2
    case tp1: AnnotatedType =>
      tp1.underlying | tp2
    case _ =>
      NoType
  }

  /** Handle `&`-conflict. If `tp2` is strictly better than `tp1` as determined
   *  by @see `isAsGood`, pick `tp2` as the winner otherwise pick `tp1`.
   *  Issue a warning and return the winner.
   */
  private def andConflict(tp1: Type, tp2: Type): Type = {
    // println(disambiguated(implicit ctx => TypeComparer.explained(_.typeComparer.isSubType(tp1, tp2)))) !!!DEBUG
    val winner = if (isAsGood(tp2, tp1) && !isAsGood(tp1, tp2)) tp2 else tp1
    def msg = disambiguated { implicit ctx =>
      s"${mergeErrorMsg(tp1, tp2)} as members of one type; keeping only ${showType(winner)}"
    }
    /* !!! DEBUG
    println("right not a subtype of left because:")
    println(TypeComparer.explained { implicit ctx => tp2 <:< tp1})
    println("left not a subtype of right because:")
    println(TypeComparer.explained { implicit ctx => tp1 <:< tp2})
    assert(false, s"andConflict ${tp1.show} and ${tp2.show}")
    */
    ctx.warning(msg, ctx.tree.pos)
    winner
  }

  /** Handle `|`-conflict by raising a `MergeError` exception */
  private def orConflict(tp1: Type, tp2: Type): Type =
    throw new MergeError(mergeErrorMsg(tp1, tp2))

  /** Merge two lists of names. If names in corresponding positions match, keep them,
   *  otherwise generate new synthetic names.
   */
  private def mergeNames[N <: Name](names1: List[N], names2: List[N], syntheticName: Int => N): List[N] = {
    for ((name1, name2, idx) <- (names1, names2, 0 until names1.length).zipped)
    yield if (name1 == name2) name1 else syntheticName(idx)
  }.toList

  /** Show type, handling type types better than the default */
  private def showType(tp: Type)(implicit ctx: Context) = tp match {
    case ClassInfo(_, cls, _, _, _) => cls.showLocated
    case bounds: TypeBounds => "type bounds" + bounds.show
    case _ => tp.show
  }

  /** The error message kernel for a merge conflict */
  private def mergeErrorMsg(tp1: Type, tp2: Type)(implicit ctx: Context) =
    s"cannot merge ${showType(tp1)} with ${showType(tp2)}"

  /** A comparison function to pick a winner in case of a merge conflict */
  private def isAsGood(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo =>
          isSubType(tp1.prefix, tp2.prefix) || (tp1.cls.owner derivesFrom tp2.cls.owner)
        case _ =>
          false
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType =>
          tp1.typeParams.length == tp2.typeParams.length &&
          isAsGood(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
    case tp1: MethodType =>
      tp2 match {
        case tp2: MethodType =>
          def asGoodParams(formals1: List[Type], formals2: List[Type]) =
            (formals2 corresponds formals1)(isSubType)
          asGoodParams(tp1.paramTypes, tp2.paramTypes) &&
          (!asGoodParams(tp2.paramTypes, tp1.paramTypes) ||
           isAsGood(tp1.resultType, tp2.resultType))
        case _ =>
          false
      }
    case _ =>
      false
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
  def subsumes(c1: Constraint, c2: Constraint, pre: Constraint): Boolean =
    if (c2 eq pre) true
    else if (c1 eq pre) false
    else {
      val saved = constraint
      try
        c2.forallParams(p => c1.contains(p) && isSubType(c1.bounds(p), c2.bounds(p)))
      finally constraint = saved
    }

  /** A new type comparer of the same type as this one, using the given context. */
  def copyIn(ctx: Context) = new TypeComparer(ctx)

  /** A hook for showing subtype traces. Overridden in ExplainingTypeComparer */
  def traceIndented[T](str: String)(op: => T): T = op
}

object TypeComparer {

  /** Show trace of comparison operations when performing `op` as result string */
  def explained[T](op: Context => T)(implicit ctx: Context): String = {
    val nestedCtx = ctx.fresh.setTypeComparerFn(new ExplainingTypeComparer(_))
    op(nestedCtx)
    nestedCtx.typeComparer.toString
  }
}

/** A type comparer that can record traces of subtype operations */
class ExplainingTypeComparer(initctx: Context) extends TypeComparer(initctx) {
  private var indent = 0
  private val b = new StringBuilder

  private var skipped = false

  override def traceIndented[T](str: String)(op: => T): T =
    if (skipped) op
    else {
      indent += 2
      b append "\n" append (" " * indent) append "==> " append str
      val res = op
      b append "\n" append (" " * indent) append "<== " append str append " = " append show(res)
      indent -= 2
      res
    }

  private def show(res: Any) = res match {
    case res: printing.Showable if !ctx.settings.Yexplainlowlevel.value => res.show
    case _ => String.valueOf(res)
  }

  override def isSubType(tp1: Type, tp2: Type) =
    traceIndented(s"${show(tp1)} <:< ${show(tp2)}${if (Config.verboseExplainSubtype) s" ${tp1.getClass} ${tp2.getClass}" else ""}${if (frozenConstraint) " frozen" else ""}") {
      super.isSubType(tp1, tp2)
    }

  override def lub(tp1: Type, tp2: Type) =
    traceIndented(s"lub(${show(tp1)}, ${show(tp2)})") {
      super.lub(tp1, tp2)
    }

  override def glb(tp1: Type, tp2: Type) =
    traceIndented(s"glb(${show(tp1)}, ${show(tp2)})") {
      super.glb(tp1, tp2)
    }

  override def addConstraint(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean =
    traceIndented(s"add constraint $param ${if (fromBelow) ">:" else "<:"} $bound $frozenConstraint") {
      super.addConstraint(param, bound, fromBelow)
    }

  override def copyIn(ctx: Context) = new ExplainingTypeComparer(ctx)

  override def toString = "Subtype trace:" + { try b.toString finally b.clear() }
}
