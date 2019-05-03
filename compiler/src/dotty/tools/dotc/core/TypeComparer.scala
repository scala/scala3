package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Flags._, Names._, NameOps._, Denotations._
import Decorators._
import StdNames.nme
import collection.mutable
import util.Stats
import config.Config
import config.Printers.{constr, subtyping, gadts, noPrinter}
import TypeErasure.{erasedLub, erasedGlb}
import TypeApplications._
import Constants.Constant
import transform.TypeUtils._
import transform.SymUtils._
import scala.util.control.NonFatal
import typer.ProtoTypes.constrained
import reporting.trace

final class AbsentContext
object AbsentContext {
  implicit val absentContext: AbsentContext = new AbsentContext
}

/** Provides methods to compare types.
 */
class TypeComparer(initctx: Context) extends ConstraintHandling[AbsentContext] {
  import TypeComparer._
  implicit def ctx(implicit nc: AbsentContext): Context = initctx

  val state = ctx.typerState
  def constraint: Constraint = state.constraint
  def constraint_=(c: Constraint): Unit = state.constraint = c

  private[this] var pendingSubTypes: mutable.Set[(Type, Type)] = null
  private[this] var recCount = 0
  private[this] var monitored = false

  private[this] var needsGc = false

  /** Is a subtype check in progress? In that case we may not
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

  /** For statistics: count how many isSubTypes are part of successful comparisons */
  private[this] var successCount = 0
  private[this] var totalCount = 0

  private[this] var myAnyClass: ClassSymbol = null
  private[this] var myAnyKindClass: ClassSymbol = null
  private[this] var myNothingClass: ClassSymbol = null
  private[this] var myNullClass: ClassSymbol = null
  private[this] var myObjectClass: ClassSymbol = null
  private[this] var myAnyType: TypeRef = null
  private[this] var myAnyKindType: TypeRef = null
  private[this] var myNothingType: TypeRef = null

  def AnyClass: ClassSymbol = {
    if (myAnyClass == null) myAnyClass = defn.AnyClass
    myAnyClass
  }
  def AnyKindClass: ClassSymbol = {
    if (myAnyKindClass == null) myAnyKindClass = defn.AnyKindClass
    myAnyKindClass
  }
  def NothingClass: ClassSymbol = {
    if (myNothingClass == null) myNothingClass = defn.NothingClass
    myNothingClass
  }
  def NullClass: ClassSymbol = {
    if (myNullClass == null) myNullClass = defn.NullClass
    myNullClass
  }
  def ObjectClass: ClassSymbol = {
    if (myObjectClass == null) myObjectClass = defn.ObjectClass
    myObjectClass
  }
  def AnyType: TypeRef = {
    if (myAnyType == null) myAnyType = AnyClass.typeRef
    myAnyType
  }
  def AnyKindType: TypeRef = {
    if (myAnyKindType == null) myAnyKindType = AnyKindClass.typeRef
    myAnyKindType
  }
  def NothingType: TypeRef = {
    if (myNothingType == null) myNothingType = NothingClass.typeRef
    myNothingType
  }

  /** Indicates whether a previous subtype check used GADT bounds */
  var GADTused: Boolean = false

  /** Record that GADT bounds of `sym` were used in a subtype check.
   *  But exclude constructor type parameters, as these are aliased
   *  to the corresponding class parameters, which does not constitute
   *  a true usage of a GADT symbol.
   */
  private def GADTusage(sym: Symbol) = {
    if (!sym.owner.isConstructor) GADTused = true
    true
  }

  protected def gadtBounds(sym: Symbol)(implicit ctx: Context) = ctx.gadt.bounds(sym)
  protected def gadtAddLowerBound(sym: Symbol, b: Type): Boolean = ctx.gadt.addBound(sym, b, isUpper = false)
  protected def gadtAddUpperBound(sym: Symbol, b: Type): Boolean = ctx.gadt.addBound(sym, b, isUpper = true)

  protected def typeVarInstance(tvar: TypeVar)(implicit ctx: Context): Type = tvar.underlying

  // Subtype testing `<:<`

  def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
    if (tp2 eq NoType) return false
    if ((tp2 eq tp1) || (tp2 eq WildcardType)) return true
    try isSubType(tp1, tp2)
    finally {
      monitored = false
      if (Config.checkConstraintsSatisfiable)
        assert(isSatisfiable, constraint.show)
    }
  }

  /** The current approximation state. See `ApproxState`. */
  private[this] var approx: ApproxState = FreshApprox
  protected def approxState: ApproxState = approx

  /** The original left-hand type of the comparison. Gets reset
   *  everytime we compare components of the previous pair of types.
   *  This type is used for capture conversion in `isSubArgs`.
   */
  private [this] var leftRoot: Type = _

  protected def isSubType(tp1: Type, tp2: Type, a: ApproxState): Boolean = {
    val savedApprox = approx
    val savedLeftRoot = leftRoot
    if (a == FreshApprox) {
      this.approx = NoApprox
      this.leftRoot = tp1
    }
    else this.approx = a
    try recur(tp1, tp2)
    catch {
      case ex: Throwable => handleRecursive("subtype", i"$tp1 <:< $tp2", ex, weight = 2)
    }
    finally {
      this.approx = savedApprox
      this.leftRoot = savedLeftRoot
    }
  }

  def isSubType(tp1: Type, tp2: Type)(implicit nc: AbsentContext): Boolean = isSubType(tp1, tp2, FreshApprox)

  /** The inner loop of the isSubType comparison.
   *  Recursive calls from recur should go to recur directly if the two types
   *  compared in the callee are essentially the same as the types compared in the
   *  caller. "The same" means: represent essentially the same sets of values.
   * `recur` should not be used to compare components of types. In this case
   *  one should use `isSubType(_, _)`.
   *  `recur` should also not be used to compare approximated versions of the original
   *  types (as when we go from an abstract type to one of its bounds). In that case
   *  one should use `isSubType(_, _, a)` where `a` defines the kind of approximation.
   *
   *  Note: Logicaly, `recur` could be nested in `isSubType`, which would avoid
   *  the instance state consisting `approx` and `leftRoot`. But then the implemented
   *  code would have two extra parameters for each of the many calls that go from
   *  one sub-part of isSubType to another.
   */
  protected def recur(tp1: Type, tp2: Type): Boolean = trace(s"isSubType ${traceInfo(tp1, tp2)} $approx", subtyping) {

    def monitoredIsSubType = {
      if (pendingSubTypes == null) {
        pendingSubTypes = new mutable.HashSet[(Type, Type)]
        ctx.log(s"!!! deep subtype recursion involving ${tp1.show} <:< ${tp2.show}, constraint = ${state.constraint.show}")
        ctx.log(s"!!! constraint = ${constraint.show}")
        //if (ctx.settings.YnoDeepSubtypes.value) {
        //  new Error("deep subtype").printStackTrace()
        //}
        assert(!ctx.settings.YnoDeepSubtypes.value)
        if (Config.traceDeepSubTypeRecursions && !this.isInstanceOf[ExplainingTypeComparer])
          ctx.log(TypeComparer.explained(implicit ctx => ctx.typeComparer.isSubType(tp1, tp2, approx)))
      }
      // Eliminate LazyRefs before checking whether we have seen a type before
      val normalize = new TypeMap {
        val DerefLimit = 10
        var derefCount = 0
        def apply(t: Type) = t match {
          case t: LazyRef =>
            // Dereference a lazyref to detect underlying matching types, but
            // be careful not to get into an infinite recursion. If recursion count
            // exceeds `DerefLimit`, approximate with `NoType` instead.
            derefCount += 1
            if (derefCount >= DerefLimit) NoType
            else try mapOver(t.ref) finally derefCount -= 1
          case tp: TypeVar =>
            tp
          case _ =>
            mapOver(t)
        }
      }
      val p = (normalize(tp1), normalize(tp2))
      !pendingSubTypes(p) && {
        try {
          pendingSubTypes += p
          firstTry
        }
        finally {
          pendingSubTypes -= p
        }
      }
    }

    def firstTry: Boolean = tp2 match {
      case tp2: NamedType =>
        def compareNamed(tp1: Type, tp2: NamedType): Boolean = {
          implicit val ctx: Context = this.ctx
          tp2.info match {
            case info2: TypeAlias => recur(tp1, info2.alias)
            case _ => tp1 match {
              case tp1: NamedType =>
                tp1.info match {
                  case info1: TypeAlias =>
                    if (recur(info1.alias, tp2)) return true
                    if (tp1.prefix.isStable) return false
                      // If tp1.prefix is stable, the alias does contain all information about the original ref, so
                      // there's no need to try something else. (This is important for performance).
                      // To see why we cannot in general stop here, consider:
                      //
                      //     trait C { type A }
                      //     trait D { type A = String }
                      //     (C & D)#A <: C#A
                      //
                      // Following the alias leads to the judgment `String <: C#A` which is false.
                      // However the original judgment should be true.
                  case _ =>
                }
                val sym2 = tp2.symbol
                var sym1 = tp1.symbol
                if (sym1.is(ModuleClass) && sym2.is(ModuleVal))
                  // For convenience we want X$ <:< X.type
                  // This is safe because X$ self-type is X.type
                  sym1 = sym1.companionModule
                if ((sym1 ne NoSymbol) && (sym1 eq sym2))
                  ctx.erasedTypes ||
                  sym1.isStaticOwner ||
                  isSubType(tp1.prefix, tp2.prefix) ||
                  thirdTryNamed(tp2)
                else
                  (  (tp1.name eq tp2.name)
                  && tp1.isMemberRef
                  && tp2.isMemberRef
                  && isSubType(tp1.prefix, tp2.prefix)
                  && tp1.signature == tp2.signature
                  && !(sym1.isClass && sym2.isClass)  // class types don't subtype each other
                  ) ||
                  thirdTryNamed(tp2)
              case _ =>
                secondTry
            }
          }
        }
        compareNamed(tp1, tp2)
      case tp2: ProtoType =>
        isMatchedByProto(tp2, tp1)
      case tp2: BoundType =>
        tp2 == tp1 || secondTry
      case tp2: TypeVar =>
        recur(tp1, typeVarInstance(tp2))
      case tp2: WildcardType =>
        def compareWild = tp2.optBounds match {
          case TypeBounds(_, hi) => recur(tp1, hi)
          case NoType => true
        }
        compareWild
      case tp2: LazyRef =>
        !tp2.evaluating && recur(tp1, tp2.ref)
      case tp2: AnnotatedType if !tp2.isRefining =>
        recur(tp1, tp2.parent)
      case tp2: ThisType =>
        def compareThis = {
          val cls2 = tp2.cls
          tp1 match {
            case tp1: ThisType =>
              // We treat two prefixes A.this, B.this as equivalent if
              // A's selftype derives from B and B's selftype derives from A.
              val cls1 = tp1.cls
              cls1.classInfo.selfType.derivesFrom(cls2) &&
              cls2.classInfo.selfType.derivesFrom(cls1)
            case tp1: NamedType if cls2.is(Module) && cls2.eq(tp1.widen.typeSymbol) =>
              cls2.isStaticOwner ||
              recur(tp1.prefix, cls2.owner.thisType) ||
              secondTry
            case _ =>
              secondTry
          }
        }
        compareThis
      case tp2: SuperType =>
        def compareSuper = tp1 match {
          case tp1: SuperType =>
            recur(tp1.thistpe, tp2.thistpe) &&
            isSameType(tp1.supertpe, tp2.supertpe)
          case _ =>
            secondTry
        }
        compareSuper
      case AndType(tp21, tp22) =>
        recur(tp1, tp21) && recur(tp1, tp22)
      case OrType(tp21, tp22) =>
        if (tp21.stripTypeVar eq tp22.stripTypeVar) recur(tp1, tp21)
        else secondTry
      case TypeErasure.ErasedValueType(tycon1, underlying2) =>
        def compareErasedValueType = tp1 match {
          case TypeErasure.ErasedValueType(tycon2, underlying1) =>
            (tycon1.symbol eq tycon2.symbol) && isSameType(underlying1, underlying2)
          case _ =>
            secondTry
        }
        compareErasedValueType
      case ConstantType(v2) =>
        tp1 match {
          case ConstantType(v1) => v1.value == v2.value
          case _ => secondTry
        }
      case tp2: AnyConstantType =>
        if (tp2.tpe.exists) recur(tp1, tp2.tpe)
        else tp1 match {
          case tp1: ConstantType =>
            tp2.tpe = tp1
            true
          case _ =>
            secondTry
        }
      case _: FlexType =>
        true
      case _ =>
        secondTry
    }

    def secondTry: Boolean = tp1 match {
      case tp1: NamedType =>
        tp1.info match {
          case info1: TypeAlias =>
            if (recur(info1.alias, tp2)) return true
            if (tp1.prefix.isStable) return false
          case _ =>
            if (tp1 eq NothingType) return true
        }
        thirdTry
      case tp1: TypeParamRef =>
        def flagNothingBound = {
          if (!frozenConstraint && tp2.isRef(NothingClass) && state.isGlobalCommittable) {
            def msg = s"!!! instantiated to Nothing: $tp1, constraint = ${constraint.show}"
            if (Config.failOnInstantiationToNothing) assert(false, msg)
            else ctx.log(msg)
          }
          true
        }
        def compareTypeParamRef =
          assumedTrue(tp1) ||
          isSubTypeWhenFrozen(bounds(tp1).hi, tp2) || {
            if (canConstrain(tp1) && !approx.high)
              addConstraint(tp1, tp2, fromBelow = false) && flagNothingBound
            else thirdTry
          }
        compareTypeParamRef
      case tp1: ThisType =>
        val cls1 = tp1.cls
        tp2 match {
          case tp2: TermRef if cls1.is(Module) && cls1.eq(tp2.widen.typeSymbol) =>
            cls1.isStaticOwner ||
            recur(cls1.owner.thisType, tp2.prefix) ||
            thirdTry
          case _ =>
            thirdTry
        }
      case tp1: SkolemType =>
        tp2 match {
          case tp2: SkolemType if !ctx.phase.isTyper && recur(tp1.info, tp2.info) => true
          case _ => thirdTry
        }
      case tp1: TypeVar =>
        recur(typeVarInstance(tp1), tp2)
      case tp1: WildcardType =>
        def compareWild = tp1.optBounds match {
          case bounds: TypeBounds => recur(bounds.lo, tp2)
          case _ => true
        }
        compareWild
      case tp1: LazyRef =>
        // If `tp1` is in train of being evaluated, don't force it
        // because that would cause an assertionError. Return false instead.
        // See i859.scala for an example where we hit this case.
        !tp1.evaluating && recur(tp1.ref, tp2)
      case tp1: AnnotatedType if !tp1.isRefining =>
        recur(tp1.parent, tp2)
      case AndType(tp11, tp12) =>
        if (tp11.stripTypeVar eq tp12.stripTypeVar) recur(tp11, tp2)
        else thirdTry
      case tp1 @ OrType(tp11, tp12) =>
        def joinOK = tp2.dealiasKeepRefiningAnnots match {
          case tp2: AppliedType if !tp2.tycon.typeSymbol.isClass =>
            // If we apply the default algorithm for `A[X] | B[Y] <: C[Z]` where `C` is a
            // type parameter, we will instantiate `C` to `A` and then fail when comparing
            // with `B[Y]`. To do the right thing, we need to instantiate `C` to the
            // common superclass of `A` and `B`.
            recur(tp1.join, tp2)
          case _ =>
            false
        }
        def widenOK =
          (tp2.widenSingletons eq tp2) &&
          (tp1.widenSingletons ne tp1) &&
          recur(tp1.widenSingletons, tp2)

        if (tp2.atoms.nonEmpty && canCompare(tp2.atoms))
          tp1.atoms.nonEmpty && tp1.atoms.subsetOf(tp2.atoms)
        else
          widenOK || joinOK || recur(tp11, tp2) && recur(tp12, tp2)
      case tp1: MatchType =>
        val reduced = tp1.reduced
        if (reduced.exists) recur(reduced, tp2) else thirdTry
      case _: FlexType =>
        true
      case _ =>
        thirdTry
    }

    def thirdTryNamed(tp2: NamedType): Boolean = tp2.info match {
      case info2: TypeBounds =>
        def compareGADT: Boolean = {
          val gbounds2 = gadtBounds(tp2.symbol)
          (gbounds2 != null) &&
            (isSubTypeWhenFrozen(tp1, gbounds2.lo) ||
              narrowGADTBounds(tp2, tp1, approx, isUpper = false)) &&
            GADTusage(tp2.symbol)
        }
        isSubApproxHi(tp1, info2.lo) || compareGADT || fourthTry

      case _ =>
        val cls2 = tp2.symbol
        if (cls2.isClass) {
          if (cls2.typeParams.isEmpty) {
            if (cls2 eq AnyKindClass) return true
            if (tp1.isRef(NothingClass)) return true
            if (tp1.isLambdaSub) return false
              // Note: We would like to replace this by `if (tp1.hasHigherKind)`
              // but right now we cannot since some parts of the standard library rely on the
              // idiom that e.g. `List <: Any`. We have to bootstrap without scalac first.
            if (cls2 eq AnyClass) return true
            if (cls2 == defn.SingletonClass && tp1.isStable) return true
            return tryBaseType(cls2)
          }
          else if (cls2.is(JavaDefined)) {
            // If `cls2` is parameterized, we are seeing a raw type, so we need to compare only the symbol
            val base = tp1.baseType(cls2)
            if (base.typeSymbol == cls2) return true
          }
          else if (tp1.isLambdaSub && !tp1.isRef(AnyKindClass))
            return recur(tp1, EtaExpansion(cls2.typeRef))
        }
        fourthTry
    }

    def thirdTry: Boolean = tp2 match {
      case tp2 @ AppliedType(tycon2, args2) =>
        compareAppliedType2(tp2, tycon2, args2)
      case tp2: NamedType =>
        thirdTryNamed(tp2)
      case tp2: TypeParamRef =>
        def compareTypeParamRef =
          assumedTrue(tp2) || {
          val alwaysTrue =
            // The following condition is carefully formulated to catch all cases
            // where the subtype relation is true without needing to add a constraint
            // It's tricky because we might need to either approximate tp2 by its
            // lower bound or else widen tp1 and check that the result is a subtype of tp2.
            // So if the constraint is not yet frozen, we do the same comparison again
            // with a frozen constraint, which means that we get a chance to do the
            // widening in `fourthTry` before adding to the constraint.
            if (frozenConstraint) recur(tp1, bounds(tp2).lo)
            else isSubTypeWhenFrozen(tp1, tp2)
          alwaysTrue ||
          frozenConstraint && (tp1 match {
            case tp1: TypeParamRef => constraint.isLess(tp1, tp2)
            case _ => false
          }) || {
            if (canConstrain(tp2) && !approx.low)
              addConstraint(tp2, tp1.widenExpr, fromBelow = true)
            else fourthTry
          }
        }
        compareTypeParamRef
      case tp2: RefinedType =>
        def compareRefinedSlow: Boolean = {
          val name2 = tp2.refinedName
          recur(tp1, tp2.parent) &&
            (name2 == nme.WILDCARD || hasMatchingMember(name2, tp1, tp2))
        }
        def compareRefined: Boolean = {
          val tp1w = tp1.widen
          val skipped2 = skipMatching(tp1w, tp2)
          if ((skipped2 eq tp2) || !Config.fastPathForRefinedSubtype)
            tp1 match {
              case tp1: AndType =>
                // Delay calling `compareRefinedSlow` because looking up a member
                // of an `AndType` can lead to a cascade of subtyping checks
                // This twist is needed to make collection/generic/ParFactory.scala compile
                fourthTry || compareRefinedSlow
              case tp1: HKTypeLambda =>
                // HKTypeLambdas do not have members.
                fourthTry
              case _ =>
                compareRefinedSlow || fourthTry
            }
          else // fast path, in particular for refinements resulting from parameterization.
            isSubRefinements(tp1w.asInstanceOf[RefinedType], tp2, skipped2) &&
            recur(tp1, skipped2)
        }
        compareRefined
      case tp2: RecType =>
        def compareRec = tp1.safeDealias match {
          case tp1: RecType =>
            val rthis1 = tp1.recThis
            recur(tp1.parent, tp2.parent.substRecThis(tp2, rthis1))
          case _ =>
            val tp1stable = ensureStableSingleton(tp1)
            recur(fixRecs(tp1stable, tp1stable.widenExpr), tp2.parent.substRecThis(tp2, tp1stable))
        }
        compareRec
      case tp2: HKTypeLambda =>
        def compareTypeLambda: Boolean = tp1.stripTypeVar match {
          case tp1: HKTypeLambda =>
           /* Don't compare bounds of lambdas under language:Scala2, or t2994 will fail.
            * The issue is that, logically, bounds should compare contravariantly,
            * but that would invalidate a pattern exploited in t2994:
            *
            *    [X0 <: Number] -> Number   <:<    [X0] -> Any
            *
            * Under the new scheme, `[X0] -> Any` is NOT a kind that subsumes
            * all other bounds. You'd have to write `[X0 >: Any <: Nothing] -> Any` instead.
            * This might look weird, but is the only logically correct way to do it.
            *
            * Note: it would be nice if this could trigger a migration warning, but I
            * am not sure how, since the code is buried so deep in subtyping logic.
            */
            def boundsOK =
              ctx.scala2Mode ||
              tp1.typeParams.corresponds(tp2.typeParams)((tparam1, tparam2) =>
                isSubType(tparam2.paramInfo.subst(tp2, tp1), tparam1.paramInfo))
            val saved = comparedTypeLambdas
            comparedTypeLambdas += tp1
            comparedTypeLambdas += tp2
            try
              variancesConform(tp1.typeParams, tp2.typeParams) &&
              boundsOK &&
              isSubType(tp1.resType, tp2.resType.subst(tp2, tp1))
            finally comparedTypeLambdas = saved
          case _ =>
            val tparams1 = tp1.typeParams
            if (tparams1.nonEmpty)
              return recur(
                HKTypeLambda.fromParams(tparams1, tp1.appliedTo(tparams1.map(_.paramRef))),
                tp2) || fourthTry
            else tp2 match {
              case EtaExpansion(tycon2) if tycon2.symbol.isClass =>
                return recur(tp1, tycon2)
              case _ =>
            }
            fourthTry
        }
        compareTypeLambda
      case OrType(tp21, tp22) =>
        if (tp2.atoms.nonEmpty && canCompare(tp2.atoms))
          return tp1.atoms.nonEmpty && tp1.atoms.subsetOf(tp2.atoms) ||
            tp1.isRef(NothingClass)

        // The next clause handles a situation like the one encountered in i2745.scala.
        // We have:
        //
        //   x: A | B, x.type <:< A | X   where X is a type variable
        //
        // We should instantiate X to B instead of x.type or A | B. To do this, we widen
        // the LHS to A | B and recur *without indicating that this is a lowApprox*. The
        // latter point is important since otherwise we would not get to instantiate X.
        // If that succeeds, fine. If not we continue and hit the `either` below.
        // That second path is important to handle comparisons with unions of singletons,
        // as in `1 <:< 1 | 2`.
        val tp1w = tp1.widen
        if ((tp1w ne tp1) && recur(tp1w, tp2))
          return true

        val tp1a = tp1.dealiasKeepRefiningAnnots
        if (tp1a ne tp1)
          // Follow the alias; this might lead to an OrType on the left which needs to be split
          return recur(tp1a, tp2)

        // Rewrite T1 <: (T211 & T212) | T22 to T1 <: (T211 | T22) and T1 <: (T212 | T22)
        // and analogously for T1 <: T21 | (T221 & T222)
        // `|' types to the right of <: are problematic, because
        // we have to choose one constraint set or another, which might cut off
        // solutions. The rewriting delays the point where we have to choose.
        tp21 match {
          case AndType(tp211, tp212) =>
            return recur(tp1, OrType(tp211, tp22)) && recur(tp1, OrType(tp212, tp22))
          case _ =>
        }
        tp22 match {
          case AndType(tp221, tp222) =>
            return recur(tp1, OrType(tp21, tp221)) && recur(tp1, OrType(tp21, tp222))
          case _ =>
        }
        either(recur(tp1, tp21), recur(tp1, tp22)) || fourthTry
      case tp2: MatchType =>
        val reduced = tp2.reduced
        if (reduced.exists) recur(tp1, reduced) else fourthTry
      case tp2: MethodType =>
        def compareMethod = tp1 match {
          case tp1: MethodType =>
            (tp1.signature consistentParams tp2.signature) &&
            matchingMethodParams(tp1, tp2) &&
            (!tp2.isImplicitMethod || tp1.isImplicitMethod) &&
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
          case _ => false
        }
        compareMethod
      case tp2: PolyType =>
        def comparePoly = tp1 match {
          case tp1: PolyType =>
            (tp1.signature consistentParams tp2.signature) &&
            matchingPolyParams(tp1, tp2) &&
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
          case _ => false
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
          case tp1 @ MethodType(Nil) => isSubType(tp1.resultType, restpe2)
          case tp1 @ ExprType(restpe1) => isSubType(restpe1, restpe2)
          case _ => fourthTry
        }
        compareExpr
      case tp2 @ TypeBounds(lo2, hi2) =>
        def compareTypeBounds = tp1 match {
          case tp1 @ TypeBounds(lo1, hi1) =>
            ((lo2 eq NothingType) || isSubType(lo2, lo1)) &&
            ((hi2 eq AnyType) && !hi1.isLambdaSub || (hi2 eq AnyKindType) || isSubType(hi1, hi2))
          case tp1: ClassInfo =>
            tp2 contains tp1
          case _ =>
            false
        }
        compareTypeBounds
      case tp2: AnnotatedType if tp2.isRefining =>
        (tp1.derivesAnnotWith(tp2.annot.sameAnnotation) || defn.isBottomType(tp1)) &&
        recur(tp1, tp2.parent)
      case ClassInfo(pre2, cls2, _, _, _) =>
        def compareClassInfo = tp1 match {
          case ClassInfo(pre1, cls1, _, _, _) =>
            (cls1 eq cls2) && isSubType(pre1, pre2)
          case _ =>
            false
        }
        compareClassInfo
      case _ =>
        fourthTry
    }

    def tryBaseType(cls2: Symbol) = {
      val base = tp1.baseType(cls2)
      if (base.exists && (base `ne` tp1))
        isSubType(base, tp2, if (tp1.isRef(cls2)) approx else approx.addLow) ||
        base.isInstanceOf[OrType] && fourthTry
          // if base is a disjunction, this might have come from a tp1 type that
          // expands to a match type. In this case, we should try to reduce the type
          // and compare the redux. This is done in fourthTry
      else fourthTry
    }

    def fourthTry: Boolean = tp1 match {
      case tp1: TypeRef =>
        tp1.info match {
          case TypeBounds(_, hi1) =>
            def compareGADT = {
              val gbounds1 = gadtBounds(tp1.symbol)
              (gbounds1 != null) &&
                (isSubTypeWhenFrozen(gbounds1.hi, tp2) ||
                narrowGADTBounds(tp1, tp2, approx, isUpper = true)) &&
                GADTusage(tp1.symbol)
            }
            isSubType(hi1, tp2, approx.addLow) || compareGADT
          case _ =>
            def isNullable(tp: Type): Boolean = tp.widenDealias match {
              case tp: TypeRef => tp.symbol.isNullableClass
              case tp: RefinedOrRecType => isNullable(tp.parent)
              case tp: AppliedType => isNullable(tp.tycon)
              case AndType(tp1, tp2) => isNullable(tp1) && isNullable(tp2)
              case OrType(tp1, tp2) => isNullable(tp1) || isNullable(tp2)
              case _ => false
            }
            val sym1 = tp1.symbol
            (sym1 eq NothingClass) && tp2.isValueTypeOrLambda ||
            (sym1 eq NullClass) && isNullable(tp2)
        }
      case tp1 @ AppliedType(tycon1, args1) =>
        compareAppliedType1(tp1, tycon1, args1)
      case tp1: SingletonType =>
        /** if `tp2 == p.type` and `p: q.type` then try `tp1 <:< q.type` as a last effort.*/
        def comparePaths = tp2 match {
          case tp2: TermRef =>
            tp2.info.widenExpr.dealias match {
              case tp2i: SingletonType =>
                recur(tp1, tp2i)
                  // see z1720.scala for a case where this can arise even in typer.
                  // Also, i1753.scala, to show why the dealias above is necessary.
              case _ => false
            }
          case _ =>
            false
        }
        isNewSubType(tp1.underlying.widenExpr) || comparePaths
      case tp1: RefinedType =>
        isNewSubType(tp1.parent)
      case tp1: RecType =>
        isNewSubType(tp1.parent)
      case tp1: HKTypeLambda =>
        def compareHKLambda = tp1 match {
          case EtaExpansion(tycon1) => recur(tycon1, tp2)
          case _ => tp2 match {
            case tp2: HKTypeLambda => false // this case was covered in thirdTry
            case _ => tp2.typeParams.hasSameLengthAs(tp1.paramRefs) && isSubType(tp1.resultType, tp2.appliedTo(tp1.paramRefs))
          }
        }
        compareHKLambda
      case AndType(tp11, tp12) =>
        val tp2a = tp2.dealiasKeepRefiningAnnots
        if (tp2a ne tp2) // Follow the alias; this might avoid truncating the search space in the either below
          return recur(tp1, tp2a)

        // Rewrite (T111 | T112) & T12 <: T2 to (T111 & T12) <: T2 and (T112 | T12) <: T2
        // and analogously for T11 & (T121 | T122) & T12 <: T2
        // `&' types to the left of <: are problematic, because
        // we have to choose one constraint set or another, which might cut off
        // solutions. The rewriting delays the point where we have to choose.
        tp11 match {
          case OrType(tp111, tp112) =>
            return recur(AndType(tp111, tp12), tp2) && recur(AndType(tp112, tp12), tp2)
          case _ =>
        }
        tp12 match {
          case OrType(tp121, tp122) =>
            return recur(AndType(tp11, tp121), tp2) && recur(AndType(tp11, tp122), tp2)
          case _ =>
        }
        val tp1norm = simplifyAndTypeWithFallback(tp11, tp12, tp1)
        if (tp1 ne tp1norm) recur(tp1norm, tp2)
        else either(recur(tp11, tp2), recur(tp12, tp2))
      case tp1: MatchType =>
        def compareMatch = tp2 match {
          case tp2: MatchType =>
            isSameType(tp1.scrutinee, tp2.scrutinee) &&
            tp1.cases.corresponds(tp2.cases)(isSubType)
          case _ => false
        }
        recur(tp1.underlying, tp2) || compareMatch
      case tp1: AnnotatedType if tp1.isRefining =>
        isNewSubType(tp1.parent)
      case JavaArrayType(elem1) =>
        def compareJavaArray = tp2 match {
          case JavaArrayType(elem2) => isSubType(elem1, elem2)
          case _ => tp2 isRef ObjectClass
        }
        compareJavaArray
      case tp1: ExprType if ctx.phase.id > ctx.gettersPhase.id =>
        // getters might have converted T to => T, need to compensate.
        recur(tp1.widenExpr, tp2)
      case _ =>
        false
    }

    /** Subtype test for the hk application `tp2 = tycon2[args2]`.
     */
    def compareAppliedType2(tp2: AppliedType, tycon2: Type, args2: List[Type]): Boolean = {
      val tparams = tycon2.typeParams
      if (tparams.isEmpty) return false // can happen for ill-typed programs, e.g. neg/tcpoly_overloaded.scala

      /** True if `tp1` and `tp2` have compatible type constructors and their
       *  corresponding arguments are subtypes relative to their variance (see `isSubArgs`).
       */
      def isMatchingApply(tp1: Type): Boolean = tp1 match {
        case AppliedType(tycon1, args1) =>
          tycon1.dealiasKeepRefiningAnnots match {
            case tycon1: TypeParamRef =>
              (tycon1 == tycon2 ||
              canConstrain(tycon1) && tryInstantiate(tycon1, tycon2)) &&
              isSubArgs(args1, args2, tp1, tparams)
            case tycon1: TypeRef =>
              tycon2.dealiasKeepRefiningAnnots match {
                case tycon2: TypeRef =>
                  val tycon1sym = tycon1.symbol
                  val tycon2sym = tycon2.symbol

                  var touchedGADTs = false
                  def gadtBoundsContain(sym: Symbol, tp: Type): Boolean = {
                    touchedGADTs = true
                    val b = gadtBounds(sym)
                    b != null && inFrozenConstraint {
                      (b.lo =:= tp) && (b.hi =:= tp)
                    }
                  }

                  val res = (
                    tycon1sym == tycon2sym ||
                    gadtBoundsContain(tycon1sym, tycon2) ||
                    gadtBoundsContain(tycon2sym, tycon1)
                  ) &&
                  isSubType(tycon1.prefix, tycon2.prefix) &&
                  isSubArgs(args1, args2, tp1, tparams)
                  if (res && touchedGADTs) GADTused = true
                  res
                case _ =>
                  false
              }
            case tycon1: TypeVar =>
              isMatchingApply(tycon1.underlying)
            case tycon1: AnnotatedType if !tycon1.isRefining =>
              isMatchingApply(tycon1.underlying)
            case _ =>
              false
          }
        case _ =>
          false
      }

      /** `param2` can be instantiated to a type application prefix of the LHS
       *  or to a type application prefix of one of the LHS base class instances
       *  and the resulting type application is a supertype of `tp1`,
       *  or fallback to fourthTry.
       */
      def canInstantiate(tycon2: TypeParamRef): Boolean = {

        /** Let
         *
         *    `tparams_1, ..., tparams_k-1`    be the type parameters of the rhs
         *    `tparams1_1, ..., tparams1_n-1`  be the type parameters of the constructor of the lhs
         *    `args1_1, ..., args1_n-1`        be the type arguments of the lhs
         *    `d  =  n - k`
         *
         *  Returns `true` iff `d >= 0` and `tycon2` can be instantiated to
         *
         *      [tparams1_d, ... tparams1_n-1] -> tycon1[args_1, ..., args_d-1, tparams_d, ... tparams_n-1]
         *
         *  such that the resulting type application is a supertype of `tp1`.
         */
        def appOK(tp1base: Type) = tp1base match {
          case tp1base: AppliedType =>
            var tycon1 = tp1base.tycon
            val args1 = tp1base.args
            val tparams1all = tycon1.typeParams
            val lengthDiff = tparams1all.length - tparams.length
            lengthDiff >= 0 && {
              val tparams1 = tparams1all.drop(lengthDiff)
              variancesConform(tparams1, tparams) && {
                if (lengthDiff > 0)
                  tycon1 = HKTypeLambda(tparams1.map(_.paramName))(
                    tl => tparams1.map(tparam => tl.integrate(tparams, tparam.paramInfo).bounds),
                    tl => tp1base.tycon.appliedTo(args1.take(lengthDiff) ++
                            tparams1.indices.toList.map(tl.paramRefs(_))))
                (assumedTrue(tycon2) || tryInstantiate(tycon2, tycon1.ensureLambdaSub)) &&
                recur(tp1, tycon1.appliedTo(args2))
              }
            }
          case _ => false
        }

        tp1.widen match {
          case tp1w: AppliedType => appOK(tp1w)
          case tp1w =>
            tp1w.typeSymbol.isClass && {
              val classBounds = tycon2.classSymbols
              def liftToBase(bcs: List[ClassSymbol]): Boolean = bcs match {
                case bc :: bcs1 =>
                  classBounds.exists(bc.derivesFrom) && appOK(tp1w.baseType(bc)) ||
                  liftToBase(bcs1)
                case _ =>
                  false
              }
              liftToBase(tp1w.baseClasses)
            } ||
            fourthTry
        }
      }

      /** Fall back to comparing either with `fourthTry` or against the lower
       *  approximation of the rhs.
       *  @param   tyconLo   The type constructor's lower approximation.
       */
      def fallback(tyconLo: Type) =
        either(fourthTry, isSubApproxHi(tp1, tyconLo.applyIfParameterized(args2)))

      /** Let `tycon2bounds` be the bounds of the RHS type constructor `tycon2`.
       *  Let `app2 = tp2` where the type constructor of `tp2` is replaced by
       *  `tycon2bounds.lo`.
       *  If both bounds are the same, continue with `tp1 <:< app2`.
       *  otherwise continue with either
       *
       *    tp1 <:< tp2    using fourthTry (this might instantiate params in tp1)
       *    tp1 <:< app2   using isSubType (this might instantiate params in tp2)
       */
      def compareLower(tycon2bounds: TypeBounds, tyconIsTypeRef: Boolean): Boolean =
        if ((tycon2bounds.lo `eq` tycon2bounds.hi) && !tycon2bounds.isInstanceOf[MatchAlias])
          if (tyconIsTypeRef) recur(tp1, tp2.superType)
          else isSubApproxHi(tp1, tycon2bounds.lo.applyIfParameterized(args2))
        else
          fallback(tycon2bounds.lo)

      tycon2 match {
        case param2: TypeParamRef =>
          isMatchingApply(tp1) ||
          canConstrain(param2) && canInstantiate(param2) ||
          compareLower(bounds(param2), tyconIsTypeRef = false)
        case tycon2: TypeRef =>
          isMatchingApply(tp1) ||
          defn.isCompiletime_S(tycon2.symbol) && compareS(tp2, tp1, fromBelow = true) || {
            tycon2.info match {
              case info2: TypeBounds =>
                compareLower(info2, tyconIsTypeRef = true)
              case info2: ClassInfo =>
                tycon2.name.toString.startsWith("Tuple") &&
                  defn.isTupleType(tp2) && recur(tp1, tp2.toNestedPairs) ||
                tryBaseType(info2.cls)
              case _ =>
                fourthTry
            }
          }
        case _: TypeVar =>
          recur(tp1, tp2.superType)
        case tycon2: AnnotatedType if !tycon2.isRefining =>
          recur(tp1, tp2.superType)
        case tycon2: AppliedType =>
          fallback(tycon2.lowerBound)
        case _ =>
          false
      }
    }

    /** Subtype test for the application `tp1 = tycon1[args1]`.
     */
    def compareAppliedType1(tp1: AppliedType, tycon1: Type, args1: List[Type]): Boolean =
      tycon1 match {
        case param1: TypeParamRef =>
          def canInstantiate = tp2 match {
            case AppliedType(tycon2, args2) =>
              tryInstantiate(param1, tycon2.ensureLambdaSub) && isSubArgs(args1, args2, tp1, tycon2.typeParams)
            case _ =>
              false
          }
          canConstrain(param1) && canInstantiate ||
            isSubType(bounds(param1).hi.applyIfParameterized(args1), tp2, approx.addLow)
        case tycon1: TypeRef =>
          val sym = tycon1.symbol
          !sym.isClass && (
            defn.isCompiletime_S(sym) && compareS(tp1, tp2, fromBelow = false) ||
            recur(tp1.superType, tp2))
        case tycon1: TypeProxy =>
          recur(tp1.superType, tp2)
        case _ =>
          false
      }

    /** Compare `tp` of form `S[arg]` with `other`, via ">:>` if fromBelow is true, "<:<" otherwise.
     *  If `arg` is a Nat constant `n`, proceed with comparing `n + 1` and `other`.
     *  Otherwise, if `other` is a Nat constant `n`, proceed with comparing `arg` and `n - 1`.
     */
    def compareS(tp: AppliedType, other: Type, fromBelow: Boolean): Boolean = tp.args match {
      case arg :: Nil =>
        natValue(arg) match {
          case Some(n) =>
            val succ = ConstantType(Constant(n + 1))
            if (fromBelow) recur(other, succ) else recur(succ, other)
          case none =>
            natValue(other) match {
              case Some(n) if n > 0 =>
                val pred = ConstantType(Constant(n - 1))
                if (fromBelow) recur(pred, arg) else recur(arg, pred)
              case none =>
                false
            }
        }
      case _ => false
    }

    /** Like tp1 <:< tp2, but returns false immediately if we know that
     *  the case was covered previously during subtyping.
     */
    def isNewSubType(tp1: Type): Boolean =
      if (isCovered(tp1) && isCovered(tp2)) {
        //println(s"useless subtype: $tp1 <:< $tp2")
        false
      } else isSubType(tp1, tp2, approx.addLow)

    def isSubApproxHi(tp1: Type, tp2: Type): Boolean =
      tp1.eq(tp2) || tp2.ne(NothingType) && isSubType(tp1, tp2, approx.addHigh)

    // begin recur
    if (tp2 eq NoType) false
    else if (tp1 eq tp2) true
    else {
      val saved = constraint
      val savedSuccessCount = successCount
      try {
        recCount = recCount + 1
        if (recCount >= Config.LogPendingSubTypesThreshold) monitored = true
        val result = if (monitored) monitoredIsSubType else firstTry
        recCount = recCount - 1
        if (!result) state.resetConstraintTo(saved)
        else if (recCount == 0 && needsGc) {
          state.gc()
          needsGc = false
        }
        if (Stats.monitored) recordStatistics(result, savedSuccessCount)
        result
      } catch {
        case NonFatal(ex) =>
          if (ex.isInstanceOf[AssertionError]) showGoal(tp1, tp2)
          recCount -= 1
          state.resetConstraintTo(saved)
          successCount = savedSuccessCount
          throw ex
      }
    }
  }

  /** Optionally, the `n` such that `tp <:< ConstantType(Constant(n: Int))` */
  def natValue(tp: Type): Option[Int] = constValue(tp) match {
    case Some(Constant(n: Int)) if n >= 0 => Some(n)
    case _ => None
  }

  /** Optionally, the constant `c` such that `tp <:< ConstantType(c)` */
  def constValue(tp: Type): Option[Constant] = {
    val ct = new AnyConstantType
    if (isSubTypeWhenFrozen(tp, ct))
      ct.tpe match {
        case ConstantType(c) => Some(c)
        case _ => None
      }
    else None
  }

  /** Check whether we can compare the given set of atoms with another to determine
   *  a subtype test between OrTypes. There is one situation where this is not
   *  the case, which has to do with SkolemTypes. TreeChecker sometimes expects two
   *  types to be equal that have different skolems. To account for this, we identify
   *  two different skolems in all phases `p`, where `p.isTyper` is false.
   *  But in that case comparing two sets of atoms that contain skolems
   *  for equality would give the wrong result, so we should not use the sets
   *  for comparisons.
   */
  def canCompare(atoms: Set[Type]): Boolean =
    ctx.phase.isTyper || {
      val hasSkolems = new ExistsAccumulator(_.isInstanceOf[SkolemType]) {
        override def stopAtStatic = true
      }
      !atoms.exists(hasSkolems(false, _))
    }

  /** Subtype test for corresponding arguments in `args1`, `args2` according to
   *  variances in type parameters `tparams2`.
   *
   *  @param  tp1       The applied type containing `args1`
   *  @param  tparams2  The type parameters of the type constructor applied to `args2`
   */
  def isSubArgs(args1: List[Type], args2: List[Type], tp1: Type, tparams2: List[ParamInfo]): Boolean = {
    /** The bounds of parameter `tparam`, where all references to type paramneters
     *  are replaced by corresponding arguments (or their approximations in the case of
     *  wildcard arguments).
     */
    def paramBounds(tparam: Symbol): TypeBounds =
      tparam.info.substApprox(tparams2.asInstanceOf[List[Symbol]], args2).bounds

    def recurArgs(args1: List[Type], args2: List[Type], tparams2: List[ParamInfo]): Boolean =
      if (args1.isEmpty) args2.isEmpty
      else args2.nonEmpty && {
        val tparam = tparams2.head
        val v = tparam.paramVariance

        /** Try a capture conversion:
         *  If the original left-hand type `leftRoot` is a path `p.type`,
         *  and the current widened left type is an application with wildcard arguments
         *  such as `C[_]`, where `X` is `C`'s type parameter corresponding to the `_` argument,
         *  compare with `C[p.X]` instead. Otherwise return `false`.
         *  Also do a capture conversion in either of the following cases:
         *
         *   - If we are after typer. We generally relax soundness requirements then.
         *     We need the relaxed condition to correctly compute overriding relationships.
         *     Missing this case led to AbstractMethod errors in the bootstrap.
         *
         *   - If we are in mode TypevarsMissContext, which means we test implicits
         *     for eligibility. In this case, we can be more permissive, since it's
         *     just a pre-check. This relaxation is needed since the full
         *     implicit typing might perform an adaptation that skolemizes the
         *     type of a synthesized tree before comparing it with an expected type.
         *     But no such adaptation is applied for implicit eligibility
         *     testing, so we have to compensate.
         *
         *  Note: Doing the capture conversion on path types is actually not necessary
         *  since we can already deal with the situation through skolemization in Typer#captureWildcards.
         *  But performance tests indicate that it's better to do it, since we avoid
         *  skolemizations, which are more expensive . And, besides, capture conversion on
         *  paths is less intrusive than skolemization.
         */
        def compareCaptured(arg1: TypeBounds, arg2: Type) = tparam match {
          case tparam: Symbol
          if leftRoot.isStable || (ctx.isAfterTyper || ctx.mode.is(Mode.TypevarsMissContext)) && leftRoot.member(tparam.name).exists =>
            val captured = TypeRef(leftRoot, tparam)
            isSubArg(captured, arg2)
          case _ =>
            false
        }

        def isSubArg(arg1: Type, arg2: Type): Boolean = arg2 match {
          case arg2: TypeBounds =>
            val arg1norm = arg1 match {
              case arg1: TypeBounds =>
                tparam match {
                  case tparam: Symbol => arg1 & paramBounds(tparam)
                  case _ => arg1 // This case can only arise when a hk-type is illegally instantiated with a wildcard
                }
              case _ => arg1
            }
            arg2.contains(arg1norm)
          case _ =>
            arg1 match {
              case arg1: TypeBounds =>
                compareCaptured(arg1, arg2)
              case _ =>
                (v > 0 || isSubType(arg2, arg1)) &&
                (v < 0 || isSubType(arg1, arg2))
            }
        }

        val arg1 = args1.head
        val arg2 = args2.head
        isSubArg(arg1, arg2) || {
          // last effort: try to adapt variances of higher-kinded types if this is sound.
          // TODO: Move this to eta-expansion?
          val adapted2 = arg2.adaptHkVariances(tparam.paramInfo)
          adapted2.ne(arg2) && isSubArg(arg1, adapted2)
        }
      } && recurArgs(args1.tail, args2.tail, tparams2.tail)

    recurArgs(args1, args2, tparams2)
  }

  /** Test whether `tp1` has a base type of the form `B[T1, ..., Tn]` where
   *   - `B` derives from one of the class symbols of `tp2`,
   *   - the type parameters of `B` match one-by-one the variances of `tparams`,
   *   - `B` satisfies predicate `p`.
   */
  private def testLifted(tp1: Type, tp2: Type, tparams: List[TypeParamInfo], p: Type => Boolean): Boolean = {
    val classBounds = tp2.classSymbols
    def recur(bcs: List[ClassSymbol]): Boolean = bcs match {
      case bc :: bcs1 =>
        (classBounds.exists(bc.derivesFrom) &&
          variancesConform(bc.typeParams, tparams) &&
          p(tp1.baseType(bc))
        ||
        recur(bcs1))
      case nil =>
        false
    }
    recur(tp1.baseClasses)
  }

  /** Replace any top-level recursive type `{ z => T }` in `tp` with
   *  `[z := anchor]T`.
   */
  private def fixRecs(anchor: SingletonType, tp: Type): Type = {
    def fix(tp: Type): Type = tp.stripTypeVar match {
      case tp: RecType => fix(tp.parent).substRecThis(tp, anchor)
      case tp @ RefinedType(parent, rname, rinfo) => tp.derivedRefinedType(fix(parent), rname, rinfo)
      case tp: TypeParamRef => fixOrElse(bounds(tp).hi, tp)
      case tp: TypeProxy => fixOrElse(tp.underlying, tp)
      case tp: AndType => tp.derivedAndType(fix(tp.tp1), fix(tp.tp2))
      case tp: OrType  => tp.derivedOrType (fix(tp.tp1), fix(tp.tp2))
      case tp => tp
    }
    def fixOrElse(tp: Type, fallback: Type) = {
      val tp1 = fix(tp)
      if (tp1 ne tp) tp1 else fallback
    }
    fix(tp)
  }

  /** Returns true iff the result of evaluating either `op1` or `op2` is true,
   *  trying at the same time to keep the constraint as wide as possible.
   *  E.g, if
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
  private def either(op1: => Boolean, op2: => Boolean): Boolean = {
    val preConstraint = constraint
    op1 && {
      val leftConstraint = constraint
      constraint = preConstraint
      if (!(op2 && subsumes(leftConstraint, constraint, preConstraint))) {
        if (constr != noPrinter && !subsumes(constraint, leftConstraint, preConstraint))
          constr.println(i"CUT - prefer $leftConstraint over $constraint")
        constraint = leftConstraint
      }
      true
    } || op2
  }

  /** Does type `tp1` have a member with name `name` whose normalized type is a subtype of
   *  the normalized type of the refinement `tp2`?
   *  Normalization is as follows: If `tp2` contains a skolem to its refinement type,
   *  rebase both itself and the member info of `tp` on a freshly created skolem type.
   */
  protected def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean =
    /*>|>*/ trace(i"hasMatchingMember($tp1 . $name :? ${tp2.refinedInfo}), mbr: ${tp1.member(name).info}", subtyping) /*<|<*/ {
      val rinfo2 = tp2.refinedInfo

      // If the member is an abstract type and the prefix is a path, compare the member itself
      // instead of its bounds. This case is needed situations like:
      //
      //    class C { type T }
      //    val foo: C
      //    foo.type <: C { type T {= , <: , >:} foo.T }
      //
      // or like:
      //
      //    class C[T]
      //    C[_] <: C[TV]
      //
      // where TV is a type variable. See i2397.scala for an example of the latter.
      def matchAbstractTypeMember(info1: Type) = info1 match {
        case TypeBounds(lo, hi) if lo ne hi =>
          tp2.refinedInfo match {
            case rinfo2: TypeBounds if tp1.isStable =>
              val ref1 = tp1.widenExpr.select(name)
              isSubType(rinfo2.lo, ref1) && isSubType(ref1, rinfo2.hi)
            case _ =>
              false
          }
        case _ => false
      }

      def qualifies(m: SingleDenotation) =
        isSubType(m.info.widenExpr, rinfo2.widenExpr) || matchAbstractTypeMember(m.info)

      tp1.member(name) match { // inlined hasAltWith for performance
        case mbr: SingleDenotation => qualifies(mbr)
        case mbr => mbr hasAltWith qualifies
      }
    }

  final def ensureStableSingleton(tp: Type): SingletonType = tp.stripTypeVar match {
    case tp: SingletonType if tp.isStable => tp
    case tp: ValueType => SkolemType(tp)
    case tp: TypeProxy => ensureStableSingleton(tp.underlying)
  }

  /** Skip refinements in `tp2` which match corresponding refinements in `tp1`.
   *  "Match" means:
   *   - they appear in the same order,
   *   - they refine the same names,
   *   - the refinement in `tp1` is an alias type, and
   *   - neither refinement refers back to the refined type via a refined this.
   *  @return  The parent type of `tp2` after skipping the matching refinements.
   */
  private def skipMatching(tp1: Type, tp2: RefinedType): Type = tp1 match {
    case tp1 @ RefinedType(parent1, name1, rinfo1: TypeAlias) if name1 == tp2.refinedName =>
      tp2.parent match {
        case parent2: RefinedType => skipMatching(parent1, parent2)
        case parent2 => parent2
      }
    case _ => tp2
  }

  /** Are refinements in `tp1` pairwise subtypes of the refinements of `tp2`
   *  up to parent type `limit`?
   *  @pre `tp1` has the necessary number of refinements, they are type aliases,
   *       and their names match the corresponding refinements in `tp2`.
   *       Further, no refinement refers back to the refined type via a refined this.
   *  The precondition is established by `skipMatching`.
   */
  private def isSubRefinements(tp1: RefinedType, tp2: RefinedType, limit: Type): Boolean = {
    def hasSubRefinement(tp1: RefinedType, refine2: Type): Boolean = {
      isSubType(tp1.refinedInfo, refine2) || {
        // last effort: try to adapt variances of higher-kinded types if this is sound.
        // TODO: Move this to eta-expansion?
        val adapted2 = refine2.adaptHkVariances(tp1.parent.member(tp1.refinedName).symbol.info)
        adapted2.ne(refine2) && hasSubRefinement(tp1, adapted2)
      }
    }
    hasSubRefinement(tp1, tp2.refinedInfo) && (
      (tp2.parent eq limit) ||
      isSubRefinements(
        tp1.parent.asInstanceOf[RefinedType], tp2.parent.asInstanceOf[RefinedType], limit))
  }

  /** A type has been covered previously in subtype checking if it
   *  is some combination of TypeRefs that point to classes, where the
   *  combiners are AppliedTypes, RefinedTypes, RecTypes, And/Or-Types or AnnotatedTypes.
   */
   private def isCovered(tp: Type): Boolean = tp.dealiasKeepRefiningAnnots.stripTypeVar match {
    case tp: TypeRef => tp.symbol.isClass && tp.symbol != NothingClass && tp.symbol != NullClass
    case tp: AppliedType => isCovered(tp.tycon)
    case tp: RefinedOrRecType => isCovered(tp.parent)
    case tp: AndType => isCovered(tp.tp1) && isCovered(tp.tp2)
    case tp: OrType  => isCovered(tp.tp1) && isCovered(tp.tp2)
    case _ => false
  }

  /** Defer constraining type variables when compared against prototypes */
  def isMatchedByProto(proto: ProtoType, tp: Type): Boolean = tp.stripTypeVar match {
    case tp: TypeParamRef if constraint contains tp => true
    case _ => proto.isMatchedBy(tp, keepConstraint = true)
  }

  /** Narrow gadt.bounds for the type parameter referenced by `tr` to include
   *  `bound` as an upper or lower bound (which depends on `isUpper`).
   *  Test that the resulting bounds are still satisfiable.
   */
  private def narrowGADTBounds(tr: NamedType, bound: Type, approx: ApproxState, isUpper: Boolean): Boolean = {
    val boundImprecise = if (isUpper) approx.high else approx.low
    ctx.mode.is(Mode.GADTflexible) && !frozenConstraint && !boundImprecise && {
      val tparam = tr.symbol
      gadts.println(i"narrow gadt bound of $tparam: ${tparam.info} from ${if (isUpper) "above" else "below"} to $bound ${bound.toString} ${bound.isRef(tparam)}")
      if (bound.isRef(tparam)) false
      else if (isUpper) gadtAddUpperBound(tparam, bound)
      else gadtAddLowerBound(tparam, bound)
    }
  }

  // Tests around `matches`

  /** A function implementing `tp1` matches `tp2`. */
  final def matchesType(tp1: Type, tp2: Type, relaxed: Boolean): Boolean = tp1.widen match {
    case tp1: MethodType =>
      tp2.widen match {
        case tp2: MethodType =>
          // implicitness is ignored when matching
          matchingMethodParams(tp1, tp2) &&
          matchesType(tp1.resultType, tp2.resultType.subst(tp2, tp1), relaxed)
        case tp2 =>
          relaxed && tp1.paramNames.isEmpty &&
            matchesType(tp1.resultType, tp2, relaxed)
      }
    case tp1: PolyType =>
      tp2.widen match {
        case tp2: PolyType =>
          sameLength(tp1.paramNames, tp2.paramNames) &&
          matchesType(tp1.resultType, tp2.resultType.subst(tp2, tp1), relaxed)
        case _ =>
          false
      }
    case _ =>
      tp2.widen match {
        case _: PolyType =>
          false
        case tp2: MethodType =>
          relaxed && tp2.paramNames.isEmpty &&
            matchesType(tp1, tp2.resultType, relaxed)
        case tp2 =>
          relaxed || isSameType(tp1, tp2)
      }
  }

  /** Do the parameter types of `tp1` and `tp2` match in a way that allows `tp1`
   *  to override `tp2` ? This is the case if they're pairwise =:=, as a special
   *  case, we allow `Any` in Java methods to match `Object`.
   */
  def matchingMethodParams(tp1: MethodType, tp2: MethodType): Boolean = {
    def loop(formals1: List[Type], formals2: List[Type]): Boolean = formals1 match {
      case formal1 :: rest1 =>
        formals2 match {
          case formal2 :: rest2 =>
            val formal2a = if (tp2.isParamDependent) formal2.subst(tp2, tp1) else formal2
            (isSameTypeWhenFrozen(formal1, formal2a)
            || tp1.isJavaMethod && (formal2 isRef ObjectClass) && (formal1 isRef AnyClass)
            || tp2.isJavaMethod && (formal1 isRef ObjectClass) && (formal2 isRef AnyClass)) &&
            loop(rest1, rest2)
          case nil =>
            false
        }
      case nil =>
        formals2.isEmpty
    }
    loop(tp1.paramInfos, tp2.paramInfos)
  }

  /** Do the parameter types of `tp1` and `tp2` match in a way that allows `tp1`
   *  to override `tp2` ? This is the case if they're pairwise >:>.
   */
  def matchingPolyParams(tp1: PolyType, tp2: PolyType): Boolean = {
    def loop(formals1: List[Type], formals2: List[Type]): Boolean = formals1 match {
      case formal1 :: rest1 =>
        formals2 match {
          case formal2 :: rest2 =>
            val formal2a = formal2.subst(tp2, tp1)
            isSubTypeWhenFrozen(formal2a, formal1) &&
            loop(rest1, rest2)
          case nil =>
            false
        }
      case nil =>
        formals2.isEmpty
    }
    loop(tp1.paramInfos, tp2.paramInfos)
  }

  // Type equality =:=

  /** Two types are the same if are mutual subtypes of each other */
  def isSameType(tp1: Type, tp2: Type)(implicit nc: AbsentContext): Boolean =
    if (tp1 eq NoType) false
    else if (tp1 eq tp2) true
    else isSubType(tp1, tp2) && isSubType(tp2, tp1)

  /** Same as `isSameType` but also can be applied to overloaded TermRefs, where
   *  two overloaded refs are the same if they have pairwise equal alternatives
   */
  def isSameRef(tp1: Type, tp2: Type): Boolean = trace(s"isSameRef($tp1, $tp2") {
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

  /** If the range `tp1..tp2` consist of a single type, that type, otherwise NoType`.
   *  This is the case if `tp1 =:= tp2`, but also if `tp1 <:< tp2`, `tp1` is a singleton type,
   *  and `tp2` derives from `scala.Singleton` (or vice-versa). Examples of the latter case:
   *
   *     "name".type .. Singleton
   *     "name".type .. String & Singleton
   *     Singleton .. "name".type
   *     String & Singleton .. "name".type
   *
   *  All consist of the single type `"name".type`.
   */
  def singletonInterval(tp1: Type, tp2: Type): Type = {
    def isSingletonBounds(lo: Type, hi: Type) =
      lo.isSingleton && hi.derivesFrom(defn.SingletonClass) && isSubTypeWhenFrozen(lo, hi)
    if (isSameTypeWhenFrozen(tp1, tp2)) tp1
    else if (isSingletonBounds(tp1, tp2)) tp1
    else if (isSingletonBounds(tp2, tp1)) tp2
    else NoType
  }

  /** The greatest lower bound of two types */
  def glb(tp1: Type, tp2: Type): Type = /*>|>*/ trace(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true) /*<|<*/ {
    if (tp1 eq tp2) tp1
    else if (!tp1.exists) tp2
    else if (!tp2.exists) tp1
    else if ((tp1 isRef AnyClass) && !tp2.isLambdaSub || (tp1 isRef AnyKindClass) || (tp2 isRef NothingClass)) tp2
    else if ((tp2 isRef AnyClass) && !tp1.isLambdaSub || (tp2 isRef AnyKindClass) || (tp1 isRef NothingClass)) tp1
    else tp2 match {  // normalize to disjunctive normal form if possible.
      case OrType(tp21, tp22) =>
        tp1 & tp21 | tp1 & tp22
      case _ =>
        tp1 match {
          case OrType(tp11, tp12) =>
            tp11 & tp2 | tp12 & tp2
          case _ =>
            val tp1a = dropIfSuper(tp1, tp2)
            if (tp1a ne tp1) glb(tp1a, tp2)
            else {
              val tp2a = dropIfSuper(tp2, tp1)
              if (tp2a ne tp2) glb(tp1, tp2a)
              else tp1 match {
                case tp1: ConstantType =>
                  tp2 match {
                    case tp2: ConstantType =>
                      // Make use of the fact that the intersection of two constant types
                      // types which are not subtypes of each other is known to be empty.
                      // Note: The same does not apply to singleton types in general.
                      // E.g. we could have a pattern match against `x.type & y.type`
                      // which might succeed if `x` and `y` happen to be the same ref
                      // at run time. It would not work to replace that with `Nothing`.
                      // However, maybe we can still apply the replacement to
                      // types which are not explicitly written.
                      NothingType
                    case _ => andType(tp1, tp2)
                  }
                case _ => andType(tp1, tp2)
              }
          }
        }
    }
  }

  /** The greatest lower bound of a list types */
  final def glb(tps: List[Type]): Type = ((AnyType: Type) /: tps)(glb)

  def widenInUnions(implicit ctx: Context): Boolean = ctx.scala2Mode || ctx.erasedTypes

  /** The least upper bound of two types
   *  @param canConstrain  If true, new constraints might be added to simplify the lub.
   *  @note  We do not admit singleton types in or-types as lubs.
   */
  def lub(tp1: Type, tp2: Type, canConstrain: Boolean = false): Type = /*>|>*/ trace(s"lub(${tp1.show}, ${tp2.show}, canConstrain=$canConstrain)", subtyping, show = true) /*<|<*/ {
    if (tp1 eq tp2) return tp1
    if (!tp1.exists) return tp1
    if (!tp2.exists) return tp2
    if ((tp1 isRef AnyClass) || (tp1 isRef AnyKindClass) || (tp2 isRef NothingClass)) return tp1
    if ((tp2 isRef AnyClass) || (tp2 isRef AnyKindClass) || (tp1 isRef NothingClass)) return tp2
    val atoms1 = tp1.atoms
    if (atoms1.nonEmpty && !widenInUnions) {
      val atoms2 = tp2.atoms
      if (atoms2.nonEmpty) {
        if (atoms1.subsetOf(atoms2)) return tp2
        if (atoms2.subsetOf(atoms1)) return tp1
        if ((atoms1 & atoms2).isEmpty) return orType(tp1, tp2)
      }
    }
    val t1 = mergeIfSuper(tp1, tp2, canConstrain)
    if (t1.exists) return t1

    val t2 = mergeIfSuper(tp2, tp1, canConstrain)
    if (t2.exists) return t2

    def widen(tp: Type) = if (widenInUnions) tp.widen else tp.widenIfUnstable
    val tp1w = widen(tp1)
    val tp2w = widen(tp2)
    if ((tp1 ne tp1w) || (tp2 ne tp2w)) lub(tp1w, tp2w)
    else orType(tp1w, tp2w) // no need to check subtypes again
  }

  /** The least upper bound of a list of types */
  final def lub(tps: List[Type]): Type =
    ((NothingType: Type) /: tps)(lub(_,_, canConstrain = false))

  /** Try to produce joint arguments for a lub `A[T_1, ..., T_n] | A[T_1', ..., T_n']` using
   *  the following strategies:
   *
   *    - if arguments are the same, that argument.
   *    - if corresponding parameter variance is co/contra-variant, the lub/glb.
   *    - otherwise a TypeBounds containing both arguments
   */
  def lubArgs(args1: List[Type], args2: List[Type], tparams: List[TypeParamInfo], canConstrain: Boolean = false): List[Type] =
    tparams match {
      case tparam :: tparamsRest =>
        val arg1 :: args1Rest = args1
        val arg2 :: args2Rest = args2
        val common = singletonInterval(arg1, arg2)
        val v = tparam.paramVariance
        val lubArg =
          if (common.exists) common
          else if (v > 0) lub(arg1.hiBound, arg2.hiBound, canConstrain)
          else if (v < 0) glb(arg1.loBound, arg2.loBound)
          else TypeBounds(glb(arg1.loBound, arg2.loBound),
                          lub(arg1.hiBound, arg2.hiBound, canConstrain))
        lubArg :: lubArgs(args1Rest, args2Rest, tparamsRest, canConstrain)
      case nil =>
        Nil
    }

  /** Try to produce joint arguments for a glb `A[T_1, ..., T_n] & A[T_1', ..., T_n']` using
   *  the following strategies:
   *
   *    - if arguments are the same, that argument.
   *    - if corresponding parameter variance is co/contra-variant, the glb/lub.
   *    - if at least one of the arguments if a TypeBounds, the union of
   *      the bounds.
   *    - if homogenizeArgs is set, and arguments can be unified by instantiating
   *      type parameters, the unified argument.
   *    - otherwise NoType
   *
   *  The unification rule is contentious because it cuts the constraint set.
   *  Therefore it is subject to Config option `alignArgsInAnd`.
   */
  def glbArgs(args1: List[Type], args2: List[Type], tparams: List[TypeParamInfo]): List[Type] =
    tparams match {
      case tparam :: tparamsRest =>
        val arg1 :: args1Rest = args1
        val arg2 :: args2Rest = args2
        val common = singletonInterval(arg1, arg2)
        val v = tparam.paramVariance
        val glbArg =
          if (common.exists) common
          else if (v > 0) glb(arg1.hiBound, arg2.hiBound)
          else if (v < 0) lub(arg1.loBound, arg2.loBound)
          else if (isBounds(arg1) || isBounds(arg2))
            TypeBounds(lub(arg1.loBound, arg2.loBound),
                       glb(arg1.hiBound, arg2.hiBound))
          else if (homogenizeArgs && !frozenConstraint && isSameType(arg1, arg2)) arg1
          else NoType
        glbArg :: glbArgs(args1Rest, args2Rest, tparamsRest)
      case nil =>
        Nil
    }

  private def recombineAnd(tp: AndType, tp1: Type, tp2: Type) =
    if (!tp1.exists) tp2
    else if (!tp2.exists) tp1
    else tp.derivedAndType(tp1, tp2)

  /** If some (&-operand of) this type is a supertype of `sub` replace it with `NoType`.
   */
  private def dropIfSuper(tp: Type, sub: Type): Type =
    if (isSubTypeWhenFrozen(sub, tp)) NoType
    else tp match {
      case tp @ AndType(tp1, tp2) =>
        recombineAnd(tp, dropIfSuper(tp1, sub), dropIfSuper(tp2, sub))
      case _ =>
        tp
    }

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
   *  @param canConstrain  If true, new constraints might be added to make the merge possible.
   */
  private def mergeIfSuper(tp1: Type, tp2: Type, canConstrain: Boolean): Type =
    if (isSubType(tp2, tp1, whenFrozen = !canConstrain))
      if (isSubType(tp1, tp2, whenFrozen = !canConstrain)) tp2 else tp1 // keep existing type if possible
    else tp2 match {
      case tp2 @ OrType(tp21, tp22) =>
        val higher1 = mergeIfSuper(tp1, tp21, canConstrain)
        if (higher1 eq tp21) tp2
        else if (higher1.exists) higher1 | tp22
        else {
          val higher2 = mergeIfSuper(tp1, tp22, canConstrain)
          if (higher2 eq tp22) tp2
          else if (higher2.exists) tp21 | higher2
          else NoType
        }
      case _ =>
        NoType
    }

  private[this] def andTypeGen(tp1: Type, tp2: Type, op: (Type, Type) => Type,
      original: (Type, Type) => Type = _ & _, isErased: Boolean = ctx.erasedTypes): Type = trace(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true) {
    val t1 = distributeAnd(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeAnd(tp2, tp1)
      if (t2.exists) t2
      else if (isErased) erasedGlb(tp1, tp2, isJava = false)
      else liftIfHK(tp1, tp2, op, original)
    }
  }

  /** Form a normalized conjunction of two types.
   *  Note: For certain types, `&` is distributed inside the type. This holds for
   *  all types which are not value types (e.g. TypeBounds, ClassInfo,
   *  ExprType, LambdaType). Also, when forming an `&`,
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
   *  In these cases, a MergeError is thrown.
   */
  final def andType(tp1: Type, tp2: Type, isErased: Boolean = ctx.erasedTypes): Type =
    andTypeGen(tp1, tp2, AndType(_, _), isErased = isErased)

  final def simplifyAndTypeWithFallback(tp1: Type, tp2: Type, fallback: Type): Type =
    andTypeGen(tp1, tp2, (_, _) => fallback)

  /** Form a normalized conjunction of two types.
   *  Note: For certain types, `|` is distributed inside the type. This holds for
   *  all types which are not value types (e.g. TypeBounds, ClassInfo,
   *  ExprType, LambdaType). Also, when forming an `|`,
   *  instantiated TypeVars are dereferenced and annotations are stripped.
   *
   *  Sometimes, the disjunction of two types cannot be formed because
   *  the types are in conflict of each other. (@see `andType` for an enumeration
   *  of these cases). In cases of conflict a `MergeError` is raised.
   *
   *  @param isErased Apply erasure semantics. If erased is true, instead of creating
   *                  an OrType, the lub will be computed using TypeCreator#erasedLub.
   */
  final def orType(tp1: Type, tp2: Type, isErased: Boolean = ctx.erasedTypes): Type = {
    val t1 = distributeOr(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeOr(tp2, tp1)
      if (t2.exists) t2
      else if (isErased) erasedLub(tp1, tp2)
      else liftIfHK(tp1, tp2, OrType(_, _), _ | _)
    }
  }

  /** `op(tp1, tp2)` unless `tp1` and `tp2` are type-constructors.
   *  In the latter case, combine `tp1` and `tp2` under a type lambda like this:
   *
   *    [X1, ..., Xn] -> op(tp1[X1, ..., Xn], tp2[X1, ..., Xn])
   */
  private def liftIfHK(tp1: Type, tp2: Type, op: (Type, Type) => Type, original: (Type, Type) => Type) = {
    val tparams1 = tp1.typeParams
    val tparams2 = tp2.typeParams
    def applied(tp: Type) = tp.appliedTo(tp.typeParams.map(_.paramInfoAsSeenFrom(tp)))
    if (tparams1.isEmpty)
      if (tparams2.isEmpty) op(tp1, tp2)
      else original(tp1, applied(tp2))
    else if (tparams2.isEmpty)
      original(applied(tp1), tp2)
    else if (tparams1.hasSameLengthAs(tparams2))
      HKTypeLambda(
        paramNames = (HKTypeLambda.syntheticParamNames(tparams1.length), tparams1, tparams2)
          .zipped.map((pname, tparam1, tparam2) =>
            pname.withVariance((tparam1.paramVariance + tparam2.paramVariance) / 2)))(
        paramInfosExp = tl => (tparams1, tparams2).zipped.map((tparam1, tparam2) =>
          tl.integrate(tparams1, tparam1.paramInfoAsSeenFrom(tp1)).bounds &
          tl.integrate(tparams2, tparam2.paramInfoAsSeenFrom(tp2)).bounds),
        resultTypeExp = tl =>
          original(tp1.appliedTo(tl.paramRefs), tp2.appliedTo(tl.paramRefs)))
    else original(applied(tp1), applied(tp2))
  }

  /** Try to distribute `&` inside type, detect and handle conflicts
   *  @pre !(tp1 <: tp2) && !(tp2 <:< tp1) -- these cases were handled before
   */
  private def distributeAnd(tp1: Type, tp2: Type): Type = tp1 match {
    case tp1 @ AppliedType(tycon1, args1) =>
      tp2 match {
        case AppliedType(tycon2, args2)
        if tycon1.typeSymbol == tycon2.typeSymbol && tycon1 =:= tycon2 =>
          val jointArgs = glbArgs(args1, args2, tycon1.typeParams)
          if (jointArgs.forall(_.exists)) (tycon1 & tycon2).appliedTo(jointArgs)
          else NoType
        case _ =>
          NoType
      }
    // opportunistically merge same-named refinements
    // this does not change anything semantically (i.e. merging or not merging
    // gives =:= types), but it keeps the type smaller.
    case tp1: RefinedType =>
      tp2 match {
        case tp2: RefinedType if tp1.refinedName == tp2.refinedName =>
          try {
            val jointInfo = Denotations.infoMeet(tp1.refinedInfo, tp2.refinedInfo, NoSymbol, NoSymbol, safeIntersection = false)
            tp1.derivedRefinedType(tp1.parent & tp2.parent, tp1.refinedName, jointInfo)
          }
          catch {
            case ex: MergeError => NoType
          }
        case _ =>
          NoType
      }
    case tp1: RecType =>
      tp1.rebind(distributeAnd(tp1.parent, tp2))
    case ExprType(rt1) =>
      tp2 match {
        case ExprType(rt2) =>
          ExprType(rt1 & rt2)
        case _ =>
          NoType
      }
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying & tp2
    case tp1: AnnotatedType if !tp1.isRefining =>
      tp1.underlying & tp2
    case _ =>
      NoType
  }

  /** Try to distribute `|` inside type, detect and handle conflicts
   *  Note that, unlike for `&`, a disjunction cannot be pushed into
   *  a refined or applied type. Example:
   *
   *     List[T] | List[U] is not the same as List[T | U].
   *
   *  The rhs is a proper supertype of the lhs.
   */
  private def distributeOr(tp1: Type, tp2: Type): Type = tp1 match {
    case ExprType(rt1) =>
      tp2 match {
        case ExprType(rt2) =>
          ExprType(rt1 | rt2)
        case _ =>
          NoType
      }
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying | tp2
    case tp1: AnnotatedType if !tp1.isRefining =>
      tp1.underlying | tp2
    case _ =>
      NoType
  }

  /** Show type, handling type types better than the default */
  private def showType(tp: Type)(implicit ctx: Context) = tp match {
    case ClassInfo(_, cls, _, _, _) => cls.showLocated
    case bounds: TypeBounds => "type bounds" + bounds.show
    case _ => tp.show
  }

  /** A comparison function to pick a winner in case of a merge conflict */
  private def isAsGood(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo =>
          isSubTypeWhenFrozen(tp1.prefix, tp2.prefix) || (tp1.cls.owner derivesFrom tp2.cls.owner)
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
            (formals2 corresponds formals1)(isSubTypeWhenFrozen)
          asGoodParams(tp1.paramInfos, tp2.paramInfos) &&
          (!asGoodParams(tp2.paramInfos, tp1.paramInfos) ||
           isAsGood(tp1.resultType, tp2.resultType))
        case _ =>
          false
      }
    case _ =>
      false
  }

  /** A new type comparer of the same type as this one, using the given context. */
  def copyIn(ctx: Context): TypeComparer = new TypeComparer(ctx)

  // ----------- Diagnostics --------------------------------------------------

  /** A hook for showing subtype traces. Overridden in ExplainingTypeComparer */
  def traceIndented[T](str: String)(op: => T): T = op

  private def traceInfo(tp1: Type, tp2: Type) =
    s"${tp1.show} <:< ${tp2.show}" + {
      if (ctx.settings.verbose.value || Config.verboseExplainSubtype) {
        s" ${tp1.getClass}, ${tp2.getClass}" +
        (if (frozenConstraint) " frozen" else "") +
        (if (ctx.mode is Mode.TypevarsMissContext) " tvars-miss-ctx" else "")
      }
      else ""
    }

  /** Show subtype goal that led to an assertion failure */
  def showGoal(tp1: Type, tp2: Type)(implicit ctx: Context): Unit = {
    println(i"assertion failure for ${show(tp1)} <:< ${show(tp2)}, frozen = $frozenConstraint")
    def explainPoly(tp: Type) = tp match {
      case tp: TypeParamRef => ctx.echo(s"TypeParamRef ${tp.show} found in ${tp.binder.show}")
      case tp: TypeRef if tp.symbol.exists => ctx.echo(s"typeref ${tp.show} found in ${tp.symbol.owner.show}")
      case tp: TypeVar => ctx.echo(s"typevar ${tp.show}, origin = ${tp.origin}")
      case _ => ctx.echo(s"${tp.show} is a ${tp.getClass}")
    }
    if (Config.verboseExplainSubtype) {
      explainPoly(tp1)
      explainPoly(tp2)
    }
  }

  /** Record statistics about the total number of subtype checks
   *  and the number of "successful" subtype checks, i.e. checks
   *  that form part of a subtype derivation tree that's ultimately successful.
   */
  def recordStatistics(result: Boolean, prevSuccessCount: Int): Unit = {
    // Stats.record(s"isSubType ${tp1.show} <:< ${tp2.show}")
    totalCount += 1
    if (result) successCount += 1 else successCount = prevSuccessCount
    if (recCount == 0) {
      Stats.record("successful subType", successCount)
      Stats.record("total subType", totalCount)
      successCount = 0
      totalCount = 0
    }
  }

  /** Returns last check's debug mode, if explicitly enabled. */
  def lastTrace(): String = ""

  /** Do `tp1` and `tp2` share a non-null inhabitant?
   *
   *  `false` implies that we found a proof; uncertainty default to `true`.
   *
   *  Proofs rely on the following properties of Scala types:
   *
   *  1. Single inheritance of classes
   *  2. Final classes cannot be extended
   *  3. ConstantTypes with distinc values are non intersecting
   *  4. There is no value of type Nothing
   */
  def intersecting(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = {
    // println(s"intersecting(${tp1.show}, ${tp2.show})")
    /** Can we enumerate all instantiations of this type? */
    def isClosedSum(tp: Symbol): Boolean =
      tp.is(Sealed) && tp.is(AbstractOrTrait) && !tp.hasAnonymousChild

    /** Splits a closed type into a disjunction of smaller types.
     *  It should hold that `tp` and `decompose(tp).reduce(_ or _)`
     *  denote the same set of values.
     */
    def decompose(sym: Symbol, tp: Type): List[Type] =
      sym.children.map(x => ctx.refineUsingParent(tp, x)).filter(_.exists)

    (tp1.dealias, tp2.dealias) match {
      case (tp1: ConstantType, tp2: ConstantType) =>
        tp1 == tp2
      case (tp1: TypeRef, tp2: TypeRef) if tp1.symbol.isClass && tp2.symbol.isClass =>
        val cls1 = tp1.classSymbol
        val cls2 = tp2.classSymbol
        if (cls1.derivesFrom(cls2) || cls2.derivesFrom(cls1)) {
          true
        } else {
          if (cls1.is(Final) || cls2.is(Final))
            // One of these types is final and they are not mutually
            // subtype, so they must be unrelated.
            false
          else if (!cls2.is(Trait) && !cls1.is(Trait))
            // Both of these types are classes and they are not mutually
            // subtype, so they must be unrelated by single inheritance
            // of classes.
            false
          else if (isClosedSum(cls1))
            decompose(cls1, tp1).exists(x => intersecting(x, tp2))
          else if (isClosedSum(cls2))
            decompose(cls2, tp2).exists(x => intersecting(x, tp1))
          else
            true
        }
      case (AppliedType(tycon1, args1), AppliedType(tycon2, args2)) if tycon1 == tycon2 =>
        // Unboxed xs.zip(ys).zip(zs).forall { case ((a, b), c) => f(a, b, c) }
        def zip_zip_forall[A, B, C](xs: List[A], ys: List[B], zs: List[C])(f: (A, B, C) => Boolean): Boolean = {
          xs match {
            case x :: xs => ys match {
              case y :: ys => zs match {
                case z :: zs => f(x, y, z) && zip_zip_forall(xs, ys, zs)(f)
                case _ => true
              }
              case _ => true
            }
            case _ => true
          }
        }
        def covariantIntersecting(tp1: Type, tp2: Type, tparam: TypeParamInfo): Boolean = {
          intersecting(tp1, tp2) || {
            // We still need to proof that `Nothing` is not a valid
            // instantiation of this type parameter. We have two ways
            // to get to that conclusion:
            // 1. `Nothing` does not conform to the type parameter's lb
            // 2. `tycon1` has a field typed with this type parameter.
            //
            // Because of separate compilation, the use of 2. is
            // limited to case classes.
            import dotty.tools.dotc.typer.Applications.productSelectorTypes
            val lowerBoundedByNothing = tparam.paramInfo.bounds.lo eq NothingType
            val typeUsedAsField =
              productSelectorTypes(tycon1, null).exists {
                case tp: TypeRef =>
                  (tp.designator: Any) == tparam // Bingo!
                case _ =>
                  false
              }
            lowerBoundedByNothing && !typeUsedAsField
          }
        }

        zip_zip_forall(args1, args2, tycon1.typeParams) {
          (arg1, arg2, tparam) =>
            val v = tparam.paramVariance
            if (v > 0)
              covariantIntersecting(arg1, arg2, tparam)
            else if (v < 0)
              // Contravariant case: a value where this type parameter is
              // instantiated to `Any` belongs to both types.
              true
            else
              covariantIntersecting(arg1, arg2, tparam) && (isSameType(arg1, arg2) || {
                // We can only trust a "no" from `isSameType` when both
                // `arg1` and `arg2` are fully instantiated.
                val fullyInstantiated = new TypeAccumulator[Boolean] {
                  override def apply(x: Boolean, t: Type) =
                    x && {
                      t match {
                        case tp: TypeRef if tp.symbol.isAbstractOrParamType => false
                        case _: SkolemType | _: TypeVar | _: TypeParamRef => false
                        case _ => foldOver(x, t)
                      }
                    }
                }
                !(fullyInstantiated.apply(true, arg1) &&
                  fullyInstantiated.apply(true, arg2))
              })
        }
      case (tp1: HKLambda, tp2: HKLambda) =>
        intersecting(tp1.resType, tp2.resType)
      case (_: HKLambda, _) =>
        // The intersection is ill kinded and therefore empty.
        false
      case (_, _: HKLambda) =>
        false
      case (tp1: OrType, _)  =>
        intersecting(tp1.tp1, tp2) || intersecting(tp1.tp2, tp2)
      case (_, tp2: OrType)  =>
        intersecting(tp1, tp2.tp1) || intersecting(tp1, tp2.tp2)
      case (tp1: AndType, _) =>
        intersecting(tp1.tp1, tp2) && intersecting(tp1.tp2, tp2) && intersecting(tp1.tp1, tp1.tp2)
      case (_, tp2: AndType) =>
        intersecting(tp1, tp2.tp1) && intersecting(tp1, tp2.tp2) && intersecting(tp2.tp1, tp2.tp2)
      case (tp1: TypeProxy, tp2: TypeProxy) =>
        intersecting(tp1.underlying, tp2) && intersecting(tp1, tp2.underlying)
      case (tp1: TypeProxy, _) =>
        intersecting(tp1.underlying, tp2)
      case (_, tp2: TypeProxy) =>
        intersecting(tp1, tp2.underlying)
      case _ =>
        true
    }
  }
}

object TypeComparer {

  /** Class for unification variables used in `natValue`. */
  private class AnyConstantType extends UncachedGroundType with ValueType {
    var tpe: Type = NoType
  }

  private[core] def show(res: Any)(implicit ctx: Context): String = res match {
    case res: printing.Showable if !ctx.settings.YexplainLowlevel.value => res.show
    case _ => String.valueOf(res)
  }

  private val LoApprox = 1
  private val HiApprox = 2

  /** The approximation state indicates how the pair of types currently compared
   *  relates to the types compared originally.
   *   - `NoApprox`: They are still the same types
   *   - `LoApprox`: The left type is approximated (i.e widened)"
   *   - `HiApprox`: The right type is approximated (i.e narrowed)"
   */
  class ApproxState(private val bits: Int) extends AnyVal {
    override def toString: String = {
      val lo = if ((bits & LoApprox) != 0) "LoApprox" else ""
      val hi = if ((bits & HiApprox) != 0) "HiApprox" else ""
      lo ++ hi
    }
    def addLow: ApproxState = new ApproxState(bits | LoApprox)
    def addHigh: ApproxState = new ApproxState(bits | HiApprox)
    def low: Boolean = (bits & LoApprox) != 0
    def high: Boolean = (bits & HiApprox) != 0
  }

  val NoApprox: ApproxState = new ApproxState(0)

  /** A special approximation state to indicate that this is the first time we
   *  compare (approximations of) this pair of types. It's converted to `NoApprox`
   *  in `isSubType`, but also leads to `leftRoot` being set there.
   */
  val FreshApprox: ApproxState = new ApproxState(4)

  /** Show trace of comparison operations when performing `op` as result string */
  def explaining[T](say: String => Unit)(op: Context => T)(implicit ctx: Context): T = {
    val nestedCtx = ctx.fresh.setTypeComparerFn(new ExplainingTypeComparer(_))
    val res = try { op(nestedCtx) } finally { say(nestedCtx.typeComparer.lastTrace()) }
    res
  }

  /** Like [[explaining]], but returns the trace instead */
  def explained[T](op: Context => T)(implicit ctx: Context): String = {
    var trace: String = null
    try { explaining(trace = _)(op) } catch { case ex: Throwable => ex.printStackTrace }
    trace
  }
}

class TrackingTypeComparer(initctx: Context) extends TypeComparer(initctx) {
  import state.constraint

  val footprint: mutable.Set[Type] = mutable.Set[Type]()

  override def bounds(param: TypeParamRef)(implicit nc: AbsentContext): TypeBounds = {
    if (param.binder `ne` caseLambda) footprint += param
    super.bounds(param)
  }

  override def addOneBound(param: TypeParamRef, bound: Type, isUpper: Boolean)(implicit nc: AbsentContext): Boolean = {
    if (param.binder `ne` caseLambda) footprint += param
    super.addOneBound(param, bound, isUpper)
  }

  override def gadtBounds(sym: Symbol)(implicit ctx: Context): TypeBounds = {
    if (sym.exists) footprint += sym.typeRef
    super.gadtBounds(sym)
  }

  override def gadtAddLowerBound(sym: Symbol, b: Type): Boolean = {
    if (sym.exists) footprint += sym.typeRef
    super.gadtAddLowerBound(sym, b)
  }

  override def gadtAddUpperBound(sym: Symbol, b: Type): Boolean = {
    if (sym.exists) footprint += sym.typeRef
    super.gadtAddUpperBound(sym, b)
  }

  override def typeVarInstance(tvar: TypeVar)(implicit ctx: Context): Type = {
    footprint += tvar
    super.typeVarInstance(tvar)
  }

  def matchCases(scrut: Type, cases: List[Type])(implicit ctx: Context): Type = {
    def paramInstances = new TypeAccumulator[Array[Type]] {
      def apply(inst: Array[Type], t: Type) = t match {
        case t @ TypeParamRef(b, n) if b `eq` caseLambda =>
          inst(n) = approximation(t, fromBelow = variance >= 0).simplified
          inst
        case _ =>
          foldOver(inst, t)
      }
    }

    def instantiateParams(inst: Array[Type]) = new TypeMap {
      def apply(t: Type) = t match {
        case t @ TypeParamRef(b, n) if b `eq` caseLambda => inst(n)
        case t: LazyRef => apply(t.ref)
        case _ => mapOver(t)
      }
    }

    /** Match a single case.
     *  @return  Some(tp)     if the match succeeds with type `tp`
     *           Some(NoType) if the match fails, and there is an overlap between pattern and scrutinee
     *           None         if the match fails and we should consider the following cases
     *                        because scrutinee and pattern do not overlap
     */
    def matchCase(cas: Type): Option[Type] = {
      val cas1 = cas match {
        case cas: HKTypeLambda =>
          caseLambda = constrained(cas)
          caseLambda.resultType
        case _ =>
          cas
      }
      val defn.MatchCase(pat, body) = cas1
      if (isSubType(scrut, pat))
        // `scrut` is a subtype of `pat`: *It's a Match!*
        Some {
          caseLambda match {
            case caseLambda: HKTypeLambda =>
              val instances = paramInstances(new Array(caseLambda.paramNames.length), pat)
              instantiateParams(instances)(body)
            case _ =>
              body
          }
        }
      else if (intersecting(scrut, pat))
        Some(NoType)
      else
        // We found a proof that `scrut` and  `pat` are incompatible.
        // The search continues.
        None
    }

    def recur(cases: List[Type]): Type = cases match {
      case cas :: cases1 => matchCase(cas).getOrElse(recur(cases1))
      case Nil => NoType
    }

    inFrozenConstraint(recur(cases))
  }
}

/** A type comparer that can record traces of subtype operations */
class ExplainingTypeComparer(initctx: Context) extends TypeComparer(initctx) {
  import TypeComparer._

  private[this] var indent = 0
  private val b = new StringBuilder

  private[this] var skipped = false

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

  override def isSubType(tp1: Type, tp2: Type, approx: ApproxState): Boolean =
    traceIndented(s"${show(tp1)} <:< ${show(tp2)}${if (Config.verboseExplainSubtype) s" ${tp1.getClass} ${tp2.getClass}" else ""} $approx ${if (frozenConstraint) " frozen" else ""}") {
      super.isSubType(tp1, tp2, approx)
    }

  override def recur(tp1: Type, tp2: Type): Boolean =
    traceIndented(s"${show(tp1)} <:< ${show(tp2)} recur ${if (frozenConstraint) " frozen" else ""}") {
      super.recur(tp1, tp2)
    }

  override def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean =
    traceIndented(s"hasMatchingMember(${show(tp1)} . $name, ${show(tp2.refinedInfo)}), member = ${show(tp1.member(name).info)}") {
      super.hasMatchingMember(name, tp1, tp2)
    }

  override def lub(tp1: Type, tp2: Type, canConstrain: Boolean = false): Type =
    traceIndented(s"lub(${show(tp1)}, ${show(tp2)}, canConstrain=$canConstrain)") {
      super.lub(tp1, tp2, canConstrain)
    }

  override def glb(tp1: Type, tp2: Type): Type =
    traceIndented(s"glb(${show(tp1)}, ${show(tp2)})") {
      super.glb(tp1, tp2)
    }

  override def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean)(implicit nc: AbsentContext): Boolean =
    traceIndented(i"add constraint $param ${if (fromBelow) ">:" else "<:"} $bound $frozenConstraint, constraint = ${ctx.typerState.constraint}") {
      super.addConstraint(param, bound, fromBelow)
    }

  override def copyIn(ctx: Context): ExplainingTypeComparer = new ExplainingTypeComparer(ctx)

  override def lastTrace(): String = "Subtype trace:" + { try b.toString finally b.clear() }
}
