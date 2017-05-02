package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Flags._, Names._, NameOps._, Denotations._
import Decorators._
import StdNames.{nme, tpnme}
import collection.mutable
import util.{Stats, DotClass, SimpleMap}
import config.Config
import config.Printers.{typr, constr, subtyping, noPrinter}
import TypeErasure.{erasedLub, erasedGlb}
import TypeApplications._
import scala.util.control.NonFatal

/** Provides methods to compare types.
 */
class TypeComparer(initctx: Context) extends DotClass with ConstraintHandling {
  implicit val ctx = initctx

  val state = ctx.typerState
  import state.constraint

  private var pendingSubTypes: mutable.Set[(Type, Type)] = null
  private var recCount = 0

  private var needsGc = false

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
  private var successCount = 0
  private var totalCount = 0

  private var myAnyClass: ClassSymbol = null
  private var myNothingClass: ClassSymbol = null
  private var myNullClass: ClassSymbol = null
  private var myObjectClass: ClassSymbol = null
  private var myAnyType: TypeRef = null
  private var myNothingType: TypeRef = null

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
  def NothingType = {
    if (myNothingType == null) myNothingType = NothingClass.typeRef
    myNothingType
  }

  /** Indicates whether a previous subtype check used GADT bounds */
  var GADTused = false

  /** Record that GADT bounds of `sym` were used in a subtype check.
   *  But exclude constructor type parameters, as these are aliased
   *  to the corresponding class parameters, which does not constitute
   *  a true usage of a GADT symbol.
   */
  private def GADTusage(sym: Symbol) = {
    if (!sym.owner.isConstructor) GADTused = true
    true
  }

  // Subtype testing `<:<`

  def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
    if (tp2 eq NoType) return false
    if ((tp2 eq tp1) || (tp2 eq WildcardType)) return true
    try isSubType(tp1, tp2)
    finally
      if (Config.checkConstraintsSatisfiable)
        assert(isSatisfiable, constraint.show)
  }

  protected def isSubType(tp1: Type, tp2: Type): Boolean = ctx.traceIndented(s"isSubType ${traceInfo(tp1, tp2)}", subtyping) {
    if (tp2 eq NoType) false
    else if (tp1 eq tp2) true
    else {
      val saved = constraint
      val savedSuccessCount = successCount
      try {
        recCount = recCount + 1
        val result =
          if (recCount < Config.LogPendingSubTypesThreshold) firstTry(tp1, tp2)
          else monitoredIsSubType(tp1, tp2)
        recCount = recCount - 1
        if (!result) constraint = saved
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
          constraint = saved
          successCount = savedSuccessCount
          throw ex
      }
    }
  }

  private def monitoredIsSubType(tp1: Type, tp2: Type) = {
    if (pendingSubTypes == null) {
      pendingSubTypes = new mutable.HashSet[(Type, Type)]
      ctx.log(s"!!! deep subtype recursion involving ${tp1.show} <:< ${tp2.show}, constraint = ${state.constraint.show}")
      ctx.log(s"!!! constraint = ${constraint.show}")
      //if (ctx.settings.YnoDeepSubtypes.value) {
      //  new Error("deep subtype").printStackTrace()
      //}
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

  private def firstTry(tp1: Type, tp2: Type): Boolean = tp2 match {
    case tp2: NamedType =>
      def compareNamed(tp1: Type, tp2: NamedType): Boolean = {
        implicit val ctx = this.ctx
        tp2.info match {
          case info2: TypeAlias => isSubType(tp1, info2.alias)
          case _ => tp1 match {
            case tp1: NamedType =>
              tp1.info match {
                case info1: TypeAlias =>
                  if (isSubType(info1.alias, tp2)) return true
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
              val sym1 =
                if (tp1.symbol.is(ModuleClass) && tp2.symbol.is(ModuleVal))
                  // For convenience we want X$ <:< X.type
                  // This is safe because X$ self-type is X.type
                  tp1.symbol.companionModule
                else
                  tp1.symbol
              if ((sym1 ne NoSymbol) && (sym1 eq tp2.symbol))
                ctx.erasedTypes ||
                sym1.isStaticOwner ||
                isSubType(tp1.prefix, tp2.prefix) ||
                thirdTryNamed(tp1, tp2)
              else
                (  (tp1.name eq tp2.name)
                && isSubType(tp1.prefix, tp2.prefix)
                && tp1.signature == tp2.signature
                && !tp1.isInstanceOf[WithFixedSym]
                && !tp2.isInstanceOf[WithFixedSym]
                ) ||
                thirdTryNamed(tp1, tp2)
            case _ =>
              secondTry(tp1, tp2)
          }
        }
      }
      compareNamed(tp1, tp2)
    case tp2: ProtoType =>
      isMatchedByProto(tp2, tp1)
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
      !tp2.evaluating && isSubType(tp1, tp2.ref)
    case tp2: AnnotatedType =>
      isSubType(tp1, tp2.tpe) // todo: refine?
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
            isSubType(tp1.prefix, cls2.owner.thisType) ||
            secondTry(tp1, tp2)
          case _ =>
            secondTry(tp1, tp2)
        }
      }
      compareThis
    case tp2: SuperType =>
      def compareSuper = tp1 match {
        case tp1: SuperType =>
          isSubType(tp1.thistpe, tp2.thistpe) &&
          isSameType(tp1.supertpe, tp2.supertpe)
        case _ =>
          secondTry(tp1, tp2)
      }
      compareSuper
    case AndType(tp21, tp22) =>
      isSubType(tp1, tp21) && isSubType(tp1, tp22)
    case OrType(tp21, tp22) =>
      if (tp21.stripTypeVar eq tp22.stripTypeVar) isSubType(tp1, tp21)
      else secondTry(tp1, tp2)
    case TypeErasure.ErasedValueType(tycon1, underlying2) =>
      def compareErasedValueType = tp1 match {
        case TypeErasure.ErasedValueType(tycon2, underlying1) =>
          (tycon1.symbol eq tycon2.symbol) && isSameType(underlying1, underlying2)
        case _ =>
          secondTry(tp1, tp2)
      }
      compareErasedValueType
    case ConstantType(v2) =>
      tp1 match {
        case ConstantType(v1) => v1.value == v2.value
        case _ => secondTry(tp1, tp2)
      }
    case _: FlexType =>
      true
    case _ =>
      secondTry(tp1, tp2)
  }

  private def secondTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: NamedType =>
      tp1.info match {
        case info1: TypeAlias =>
          if (isSubType(info1.alias, tp2)) return true
          if (tp1.prefix.isStable) return false
        case _ =>
      }
      thirdTry(tp1, tp2)
    case tp1: TypeParamRef =>
      def flagNothingBound = {
        if (!frozenConstraint && tp2.isRef(defn.NothingClass) && state.isGlobalCommittable) {
          def msg = s"!!! instantiated to Nothing: $tp1, constraint = ${constraint.show}"
          if (Config.failOnInstantiationToNothing) assert(false, msg)
          else ctx.log(msg)
        }
        true
      }
      def compareTypeParamRef =
        ctx.mode.is(Mode.TypevarsMissContext) ||
        isSubTypeWhenFrozen(bounds(tp1).hi, tp2) || {
          if (canConstrain(tp1)) addConstraint(tp1, tp2, fromBelow = false) && flagNothingBound
          else thirdTry(tp1, tp2)
        }
      compareTypeParamRef
    case tp1: ThisType =>
      val cls1 = tp1.cls
      tp2 match {
        case tp2: TermRef if cls1.is(Module) && cls1.eq(tp2.widen.typeSymbol) =>
          cls1.isStaticOwner ||
          isSubType(cls1.owner.thisType, tp2.prefix) ||
          thirdTry(tp1, tp2)
        case _ =>
          thirdTry(tp1, tp2)
      }
    case tp1: SkolemType =>
      tp2 match {
        case tp2: SkolemType if !ctx.phase.isTyper && tp1.info <:< tp2.info => true
        case _ => thirdTry(tp1, tp2)
      }
    case tp1: TypeVar =>
      isSubType(tp1.underlying, tp2)
    case tp1: WildcardType =>
      def compareWild = tp1.optBounds match {
        case TypeBounds(lo, _) => isSubType(lo, tp2)
        case _ => true
      }
      compareWild
    case tp1: LazyRef =>
      // If `tp1` is in train of being evaluated, don't force it
      // because that would cause an assertionError. Return false instead.
      // See i859.scala for an example where we hit this case.
      !tp1.evaluating && isSubType(tp1.ref, tp2)
    case tp1: AnnotatedType =>
      isSubType(tp1.tpe, tp2)
    case AndType(tp11, tp12) =>
      if (tp11.stripTypeVar eq tp12.stripTypeVar) isSubType(tp11, tp2)
      else thirdTry(tp1, tp2)
    case tp1 @ OrType(tp11, tp12) =>
      def joinOK = tp2.dealias match {
        case _: HKApply =>
          // If we apply the default algorithm for `A[X] | B[Y] <: C[Z]` where `C` is a
          // type parameter, we will instantiate `C` to `A` and then fail when comparing
          // with `B[Y]`. To do the right thing, we need to instantiate `C` to the
          // common superclass of `A` and `B`.
          isSubType(tp1.join, tp2)
        case _ =>
          false
      }
      joinOK || isSubType(tp11, tp2) && isSubType(tp12, tp2)
    case _: FlexType =>
      true
    case _ =>
      thirdTry(tp1, tp2)
  }

  private def thirdTryNamed(tp1: Type, tp2: NamedType): Boolean = tp2.info match {
    case TypeBounds(lo2, _) =>
      def compareGADT: Boolean = {
        val gbounds2 = ctx.gadt.bounds(tp2.symbol)
        (gbounds2 != null) &&
          (isSubTypeWhenFrozen(tp1, gbounds2.lo) ||
            narrowGADTBounds(tp2, tp1, isUpper = false)) &&
          GADTusage(tp2.symbol)
      }
      ((frozenConstraint || !isCappable(tp1)) && isSubType(tp1, lo2) ||
        compareGADT ||
        fourthTry(tp1, tp2))

    case _ =>
      val cls2 = tp2.symbol
      if (cls2.isClass) {
        val base = tp1.baseTypeRef(cls2)
        if (base.exists && (base ne tp1)) return isSubType(base, tp2)
        if (cls2 == defn.SingletonClass && tp1.isStable) return true
      }
      fourthTry(tp1, tp2)
  }

  private def thirdTry(tp1: Type, tp2: Type): Boolean = tp2 match {
    case tp2: NamedType =>
      thirdTryNamed(tp1, tp2)
    case tp2: TypeParamRef =>
      def compareTypeParamRef =
        (ctx.mode is Mode.TypevarsMissContext) || {
        val alwaysTrue =
          // The following condition is carefully formulated to catch all cases
          // where the subtype relation is true without needing to add a constraint
          // It's tricky because we might need to either appriximate tp2 by its
          // lower bound or else widen tp1 and check that the result is a subtype of tp2.
          // So if the constraint is not yet frozen, we do the same comparison again
          // with a frozen constraint, which means that we get a chance to do the
          // widening in `fourthTry` before adding to the constraint.
          if (frozenConstraint) isSubType(tp1, bounds(tp2).lo)
          else isSubTypeWhenFrozen(tp1, tp2)
        alwaysTrue || {
          if (canConstrain(tp2)) addConstraint(tp2, tp1.widenExpr, fromBelow = true)
          else fourthTry(tp1, tp2)
        }
      }
      compareTypeParamRef
    case tp2: RefinedType =>
      def compareRefinedSlow: Boolean = {
        val name2 = tp2.refinedName
        isSubType(tp1, tp2.parent) &&
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
              fourthTry(tp1, tp2) || compareRefinedSlow
            case _ =>
              compareRefinedSlow || fourthTry(tp1, tp2)
          }
        else // fast path, in particular for refinements resulting from parameterization.
          isSubRefinements(tp1w.asInstanceOf[RefinedType], tp2, skipped2) &&
          isSubType(tp1, skipped2)
      }
      compareRefined
    case tp2: RecType =>
      def compareRec = tp1.safeDealias match {
        case tp1: RecType =>
          val rthis1 = RecThis(tp1)
          isSubType(tp1.parent, tp2.parent.substRecThis(tp2, rthis1))
        case _ =>
          val tp1stable = ensureStableSingleton(tp1)
          isSubType(fixRecs(tp1stable, tp1stable.widenExpr), tp2.parent.substRecThis(tp2, tp1stable))
      }
      compareRec
    case tp2 @ HKApply(tycon2, args2) =>
      compareHkApply2(tp1, tp2, tycon2, args2)
    case tp2: HKTypeLambda =>
      def compareTypeLambda: Boolean = tp1.stripTypeVar match {
        case tp1: HKTypeLambda =>
          /* Don't compare bounds of lambdas under language:Scala2, or t2994 will fail
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
          if (!tp1.isHK) {
            tp2 match {
              case EtaExpansion(tycon2) if tycon2.symbol.isClass =>
                return isSubType(tp1, tycon2)
              case _ =>
            }
          }
          fourthTry(tp1, tp2)
      }
      compareTypeLambda
    case OrType(tp21, tp22) =>
      // Rewrite T1 <: (T211 & T212) | T22 to T1 <: (T211 | T22) and T1 <: (T212 | T22)
      // and analogously for T1 <: T21 | (T221 & T222)
      // `|' types to the right of <: are problematic, because
      // we have to choose one constraint set or another, which might cut off
      // solutions. The rewriting delays the point where we have to choose.
      tp21 match {
        case AndType(tp211, tp212) =>
          return isSubType(tp1, OrType(tp211, tp22)) && isSubType(tp1, OrType(tp212, tp22))
        case _ =>
      }
      tp22 match {
        case AndType(tp221, tp222) =>
          return isSubType(tp1, OrType(tp21, tp221)) && isSubType(tp1, OrType(tp21, tp222))
        case _ =>
      }
      either(isSubType(tp1, tp21), isSubType(tp1, tp22)) || fourthTry(tp1, tp2)
    case tp2: MethodOrPoly =>
      def compareMethod = tp1 match {
        case tp1: MethodOrPoly =>
          (tp1.signature consistentParams tp2.signature) &&
            matchingParams(tp1, tp2) &&
            tp1.isImplicit == tp2.isImplicit &&
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
      compareMethod
    case tp2 @ ExprType(restpe2) =>
      def compareExpr = tp1 match {
        // We allow ()T to be a subtype of => T.
        // We need some subtype relationship between them so that e.g.
        // def toString   and   def toString()   don't clash when seen
        // as members of the same type. And it seems most logical to take
        // ()T <:< => T, since everything one can do with a => T one can
        // also do with a ()T by automatic () insertion.
        case tp1 @ MethodType(Nil) => isSubType(tp1.resultType, restpe2)
        case _ => isSubType(tp1.widenExpr, restpe2)
      }
      compareExpr
    case tp2 @ TypeBounds(lo2, hi2) =>
      def compareTypeBounds = tp1 match {
        case tp1 @ TypeBounds(lo1, hi1) =>
          (tp2.variance > 0 && tp1.variance >= 0 || (lo2 eq NothingType) || isSubType(lo2, lo1)) &&
          (tp2.variance < 0 && tp1.variance <= 0 || (hi2 eq AnyType) || isSubType(hi1, hi2))
        case tp1: ClassInfo =>
          tp2 contains tp1
        case _ =>
          false
      }
      compareTypeBounds
    case ClassInfo(pre2, cls2, _, _, _) =>
      def compareClassInfo = tp1 match {
        case ClassInfo(pre1, cls1, _, _, _) =>
          (cls1 eq cls2) && isSubType(pre1, pre2)
        case _ =>
          false
      }
      compareClassInfo
    case _ =>
      fourthTry(tp1, tp2)
  }

  private def fourthTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: TypeRef =>
      tp1.info match {
        case TypeBounds(_, hi1) =>
          def compareGADT = {
            val gbounds1 = ctx.gadt.bounds(tp1.symbol)
            (gbounds1 != null) &&
              (isSubTypeWhenFrozen(gbounds1.hi, tp2) ||
               narrowGADTBounds(tp1, tp2, isUpper = true)) &&
              GADTusage(tp1.symbol)
          }
          isSubType(hi1, tp2) || compareGADT
        case _ =>
          def isNullable(tp: Type): Boolean = tp.widenDealias match {
            case tp: TypeRef => tp.symbol.isNullableClass
            case tp: RefinedOrRecType => isNullable(tp.parent)
            case AndType(tp1, tp2) => isNullable(tp1) && isNullable(tp2)
            case OrType(tp1, tp2) => isNullable(tp1) || isNullable(tp2)
            case _ => false
          }
          (tp1.symbol eq NothingClass) && tp2.isValueTypeOrLambda ||
          (tp1.symbol eq NullClass) && isNullable(tp2)
      }
    case tp1: SingletonType =>
      /** if `tp2 == p.type` and `p: q.type` then try `tp1 <:< q.type` as a last effort.*/
      def comparePaths = tp2 match {
        case tp2: TermRef =>
          tp2.info.widenExpr.dealias match {
            case tp2i: SingletonType =>
              isSubType(tp1, tp2i)
                // see z1720.scala for a case where this can arise even in typer.
                // Also, i1753.scala, to show why the dealias above is necessary.
            case _ => false
          }
        case _ =>
          false
      }
      isNewSubType(tp1.underlying.widenExpr, tp2) || comparePaths
    case tp1: RefinedType =>
      isNewSubType(tp1.parent, tp2)
    case tp1: RecType =>
      isNewSubType(tp1.parent, tp2)
    case tp1 @ HKApply(tycon1, args1) =>
      compareHkApply1(tp1, tycon1, args1, tp2)
    case tp1: HKTypeLambda =>
      def compareHKLambda = tp1 match {
        case EtaExpansion(tycon1) => isSubType(tycon1, tp2)
        case _ => tp2 match {
          case tp2: HKTypeLambda => false // this case was covered in thirdTry
          case _ => tp2.isHK && isSubType(tp1.resultType, tp2.appliedTo(tp1.paramRefs))
        }
      }
      compareHKLambda
    case AndType(tp11, tp12) =>
      // Rewrite (T111 | T112) & T12 <: T2 to (T111 & T12) <: T2 and (T112 | T12) <: T2
      // and analogously for T11 & (T121 | T122) & T12 <: T2
      // `&' types to the left of <: are problematic, because
      // we have to choose one constraint set or another, which might cut off
      // solutions. The rewriting delays the point where we have to choose.
      tp11 match {
        case OrType(tp111, tp112) =>
          return isSubType(AndType(tp111, tp12), tp2) && isSubType(AndType(tp112, tp12), tp2)
        case _ =>
      }
      tp12 match {
        case OrType(tp121, tp122) =>
          return isSubType(AndType(tp11, tp121), tp2) && isSubType(AndType(tp11, tp122), tp2)
        case _ =>
      }
      either(isSubType(tp11, tp2), isSubType(tp12, tp2))
    case JavaArrayType(elem1) =>
      def compareJavaArray = tp2 match {
        case JavaArrayType(elem2) => isSubType(elem1, elem2)
        case _ => tp2 isRef ObjectClass
      }
      compareJavaArray
    case tp1: ExprType if ctx.phase.id > ctx.gettersPhase.id =>
      // getters might have converted T to => T, need to compensate.
      isSubType(tp1.widenExpr, tp2)
    case _ =>
      false
  }

  /** Subtype test for the hk application `tp2 = tycon2[args2]`.
   */
  def compareHkApply2(tp1: Type, tp2: HKApply, tycon2: Type, args2: List[Type]): Boolean = {
    val tparams = tycon2.typeParams
    if (tparams.isEmpty) return false // can happen for ill-typed programs, e.g. neg/tcpoly_overloaded.scala

    /** True if `tp1` and `tp2` have compatible type constructors and their
     *  corresponding arguments are subtypes relative to their variance (see `isSubArgs`).
     */
    def isMatchingApply(tp1: Type): Boolean = tp1 match {
      case HKApply(tycon1, args1) =>
        tycon1.dealias match {
          case tycon1: TypeParamRef =>
            (tycon1 == tycon2 ||
             canConstrain(tycon1) && tryInstantiate(tycon1, tycon2)) &&
            isSubArgs(args1, args2, tparams)
          case tycon1: TypeRef =>
            tycon2.dealias match {
              case tycon2: TypeRef if tycon1.symbol == tycon2.symbol =>
                isSubType(tycon1.prefix, tycon2.prefix) &&
                isSubArgs(args1, args2, tparams)
              case _ =>
                false
            }
          case tycon1: TypeVar =>
            isMatchingApply(tycon1.underlying)
          case tycon1: AnnotatedType =>
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
       *      [tparams1_d, ... tparams1_n-1] -> tycon1a[args_1, ..., args_d-1, tparams_d, ... tparams_n-1]
       *
       *  such that the resulting type application is a supertype of `tp1`.
       */
      def tyconOK(tycon1a: Type, args1: List[Type]) = {
        var tycon1b = tycon1a
        val tparams1a = tycon1a.typeParams
        val lengthDiff = tparams1a.length - tparams.length
        lengthDiff >= 0 && {
          val tparams1 = tparams1a.drop(lengthDiff)
          variancesConform(tparams1, tparams) && {
            if (lengthDiff > 0)
              tycon1b = HKTypeLambda(tparams1.map(_.paramName))(
                tl => tparams1.map(tparam => tl.integrate(tparams, tparam.paramInfo).bounds),
                tl => tycon1a.appliedTo(args1.take(lengthDiff) ++
                        tparams1.indices.toList.map(TypeParamRef(tl, _))))
            (ctx.mode.is(Mode.TypevarsMissContext) ||
              tryInstantiate(tycon2, tycon1b.ensureHK)) &&
              isSubType(tp1, tycon1b.appliedTo(args2))
          }
        }
      }

      tp1.widen match {
        case tp1w @ HKApply(tycon1, args1) =>
          tyconOK(tycon1, args1)
        case tp1w =>
          tp1w.typeSymbol.isClass && {
            val classBounds = tycon2.classSymbols
            def liftToBase(bcs: List[ClassSymbol]): Boolean = bcs match {
              case bc :: bcs1 =>
                classBounds.exists(bc.derivesFrom) &&
                tyconOK(tp1w.baseTypeRef(bc), tp1w.baseArgInfos(bc)) ||
                liftToBase(bcs1)
              case _ =>
                false
            }
            liftToBase(tp1w.baseClasses)
          } ||
          fourthTry(tp1, tp2)
      }
    }

    /** Fall back to comparing either with `fourthTry` or against the lower
     *  approximation of the rhs.
     *  @param   tyconLo   The type constructor's lower approximation.
     */
    def fallback(tyconLo: Type) =
      either(fourthTry(tp1, tp2), isSubType(tp1, tyconLo.applyIfParameterized(args2)))

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
      if (tycon2bounds.lo eq tycon2bounds.hi)
        isSubType(tp1,
            if (tyconIsTypeRef) tp2.superType
            else tycon2bounds.lo.applyIfParameterized(args2))
      else
        fallback(tycon2bounds.lo)

    tycon2 match {
      case param2: TypeParamRef =>
        isMatchingApply(tp1) ||
        canConstrain(param2) && canInstantiate(param2) ||
        compareLower(bounds(param2), tyconIsTypeRef = false)
      case tycon2: TypeRef =>
        isMatchingApply(tp1) ||
        compareLower(tycon2.info.bounds, tyconIsTypeRef = true)
      case _: TypeVar | _: AnnotatedType =>
        isSubType(tp1, tp2.superType)
      case tycon2: HKApply =>
        fallback(tycon2.lowerBound)
      case _ =>
        false
    }
  }

  /** Subtype test for the hk application `tp1 = tycon1[args1]`.
   */
  def compareHkApply1(tp1: HKApply, tycon1: Type, args1: List[Type], tp2: Type): Boolean =
    tycon1 match {
      case param1: TypeParamRef =>
        def canInstantiate = tp2 match {
          case AppliedType(tycon2, args2) =>
            tryInstantiate(param1, tycon2.ensureHK) && isSubArgs(args1, args2, tycon2.typeParams)
          case _ =>
            false
        }
        canConstrain(param1) && canInstantiate ||
          isSubType(bounds(param1).hi.applyIfParameterized(args1), tp2)
      case tycon1: TypeProxy =>
        isSubType(tp1.superType, tp2)
      case _ =>
        false
    }

  /** Subtype test for corresponding arguments in `args1`, `args2` according to
   *  variances in type parameters `tparams`.
   */
  def isSubArgs(args1: List[Type], args2: List[Type], tparams: List[ParamInfo]): Boolean =
    if (args1.isEmpty) args2.isEmpty
    else args2.nonEmpty && {
      val v = tparams.head.paramVariance
      def isSub(tp1: Type, tp2: Type) = tp2 match {
        case tp2: TypeBounds =>
          tp2.contains(tp1)
        case _ =>
          (v > 0 || isSubType(tp2, tp1)) &&
          (v < 0 || isSubType(tp1, tp2))
      }
      isSub(args1.head, args2.head)
    } && isSubArgs(args1.tail, args2.tail, tparams.tail)

  /** Test whether `tp1` has a base type of the form `B[T1, ..., Tn]` where
   *   - `B` derives from one of the class symbols of `tp2`,
   *   - the type parameters of `B` match one-by-one the variances of `tparams`,
   *   - `B` satisfies predicate `p`.
   */
  private def testLifted(tp1: Type, tp2: Type, tparams: List[TypeParamInfo], p: Type => Boolean): Boolean = {
    val classBounds = tp2.classSymbols
    def recur(bcs: List[ClassSymbol]): Boolean = bcs match {
      case bc :: bcs1 =>
        val baseRef = tp1.baseTypeRef(bc)
        (classBounds.exists(bc.derivesFrom) &&
         variancesConform(baseRef.typeParams, tparams) &&
         p(baseRef.appliedTo(tp1.baseArgInfos(bc)))
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
      case tp: AndOrType => tp.derivedAndOrType(fix(tp.tp1), fix(tp.tp2))
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

  /** Like tp1 <:< tp2, but returns false immediately if we know that
   *  the case was covered previously during subtyping.
   */
  private def isNewSubType(tp1: Type, tp2: Type): Boolean =
    if (isCovered(tp1) && isCovered(tp2)) {
      //println(s"useless subtype: $tp1 <:< $tp2")
      false
    } else isSubType(tp1, tp2)

  /** Does type `tp1` have a member with name `name` whose normalized type is a subtype of
   *  the normalized type of the refinement `tp2`?
   *  Normalization is as follows: If `tp2` contains a skolem to its refinement type,
   *  rebase both itself and the member info of `tp` on a freshly created skolem type.
   */
  protected def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean = {
    val rinfo2 = tp2.refinedInfo
    val mbr = tp1.member(name)

    def qualifies(m: SingleDenotation) = isSubType(m.info, rinfo2)

    def memberMatches: Boolean = mbr match { // inlined hasAltWith for performance
      case mbr: SingleDenotation => qualifies(mbr)
      case _ => mbr hasAltWith qualifies
    }

    // special case for situations like:
    //    class C { type T }
    //    val foo: C
    //    foo.type <: C { type T {= , <: , >:} foo.T }
    def selfReferentialMatch = tp1.isInstanceOf[SingletonType] && {
      rinfo2 match {
        case rinfo2: TypeBounds =>
          val mbr1 = tp1.select(name)
          !defn.isBottomType(tp1.widen) &&
          (mbr1 =:= rinfo2.hi || (rinfo2.hi ne rinfo2.lo) && mbr1 =:= rinfo2.lo)
        case _ => false
      }
    }

    /*>|>*/ ctx.traceIndented(i"hasMatchingMember($tp1 . $name :? ${tp2.refinedInfo}) ${mbr.info.show} $rinfo2", subtyping) /*<|<*/ {
      memberMatches || selfReferentialMatch
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
   *  combiners are RefinedTypes, RecTypes, AndTypes or AnnotatedTypes.
   *  One exception: Refinements referring to basetype args are never considered
   *  to be already covered. This is necessary because such refined types might
   *  still need to be compared with a compareAliasRefined.
   */
  private def isCovered(tp: Type): Boolean = tp.dealias.stripTypeVar match {
    case tp: TypeRef => tp.symbol.isClass && tp.symbol != NothingClass && tp.symbol != NullClass
    case tp: ProtoType => false
    case tp: RefinedOrRecType => isCovered(tp.parent)
    case tp: AnnotatedType => isCovered(tp.underlying)
    case AndType(tp1, tp2) => isCovered(tp1) && isCovered(tp2)
    case _ => false
  }

  /** Defer constraining type variables when compared against prototypes */
  def isMatchedByProto(proto: ProtoType, tp: Type) = tp.stripTypeVar match {
    case tp: TypeParamRef if constraint contains tp => true
    case _ => proto.isMatchedBy(tp)
  }

  /** Can type `tp` be constrained from above by adding a constraint to
   *  a typevar that it refers to? In that case we have to be careful not
   *  to approximate with the lower bound of a type in `thirdTry`. Instead,
   *  we should first unroll `tp1` until we hit the type variable and bind the
   *  type variable with (the corresponding type in) `tp2` instead.
   */
  private def isCappable(tp: Type): Boolean = tp match {
    case tp: TypeParamRef => constraint contains tp
    case tp: TypeProxy => isCappable(tp.underlying)
    case tp: AndOrType => isCappable(tp.tp1) || isCappable(tp.tp2)
    case _ => false
  }

  /** Narrow gadt.bounds for the type parameter referenced by `tr` to include
   *  `bound` as an upper or lower bound (which depends on `isUpper`).
   *  Test that the resulting bounds are still satisfiable.
   */
  private def narrowGADTBounds(tr: NamedType, bound: Type, isUpper: Boolean): Boolean =
    ctx.mode.is(Mode.GADTflexible) && !frozenConstraint && {
      val tparam = tr.symbol
      typr.println(i"narrow gadt bound of $tparam: ${tparam.info} from ${if (isUpper) "above" else "below"} to $bound ${bound.isRef(tparam)}")
      if (bound.isRef(tparam)) false
      else bound match {
        case bound: TypeRef
        if bound.symbol.is(BindDefinedType) &&
           ctx.gadt.bounds.contains(bound.symbol) &&
           !tr.symbol.is(BindDefinedType) =>
          // Avoid having pattern-bound types in gadt bounds,
          // as these might be eliminated once the pattern is typechecked.
          // Pattern-bound type symbols should be narrowed first, only if that fails
          // should symbols in the environment be constrained.
          narrowGADTBounds(bound, tr, !isUpper)
        case _ =>
          val oldBounds = ctx.gadt.bounds(tparam)
          val newBounds =
            if (isUpper) TypeBounds(oldBounds.lo, oldBounds.hi & bound)
            else TypeBounds(oldBounds.lo | bound, oldBounds.hi)
          isSubType(newBounds.lo, newBounds.hi) &&
            { ctx.gadt.setBounds(tparam, newBounds); true }
      }
    }

  // Tests around `matches`

  /** A function implementing `tp1` matches `tp2`. */
  final def matchesType(tp1: Type, tp2: Type, relaxed: Boolean): Boolean = tp1.widen match {
    case tp1: MethodType =>
      tp2.widen match {
        case tp2: MethodType =>
          // implicitness is ignored when matching
          matchingParams(tp1, tp2) &&
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

  /** Do lambda types `lam1` and `lam2` have parameters that have the same types
   *  and the same implicit status? (after renaming one set to the other)
   */
  def matchingParams(lam1: MethodOrPoly, lam2: MethodOrPoly): Boolean = {
    /** Are `syms1` and `syms2` parameter lists with pairwise equivalent types? */
    def loop(formals1: List[Type], formals2: List[Type]): Boolean = formals1 match {
      case formal1 :: rest1 =>
        formals2 match {
          case formal2 :: rest2 =>
            val formal2a = if (lam2.isParamDependent) formal2.subst(lam2, lam1) else formal2
            (isSameTypeWhenFrozen(formal1, formal2a)
            || lam1.isJava && (formal2 isRef ObjectClass) && (formal1 isRef AnyClass)
            || lam2.isJava && (formal1 isRef ObjectClass) && (formal2 isRef AnyClass)) &&
            loop(rest1, rest2)
          case nil =>
            false
        }
      case nil =>
        formals2.isEmpty
    }
    loop(lam1.paramInfos, lam2.paramInfos)
  }

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
                      defn.NothingType
                    case _ => andType(tp1, tp2)
                  }
                case _ => andType(tp1, tp2)
              }
          }
        }
    }
  }

  /** The greatest lower bound of a list types */
  final def glb(tps: List[Type]): Type =
    ((defn.AnyType: Type) /: tps)(glb)

  /** The least upper bound of two types
   *  @param canConstrain  If true, new constraints might be added to simplify the lub.
   *  @note  We do not admit singleton types in or-types as lubs.
   */
  def lub(tp1: Type, tp2: Type, canConstrain: Boolean = false): Type = /*>|>*/ ctx.traceIndented(s"lub(${tp1.show}, ${tp2.show}, canConstrain=$canConstrain)", subtyping, show = true) /*<|<*/ {
    if (tp1 eq tp2) tp1
    else if (!tp1.exists) tp1
    else if (!tp2.exists) tp2
    else if ((tp1 isRef AnyClass) || (tp2 isRef NothingClass)) tp1
    else if ((tp2 isRef AnyClass) || (tp1 isRef NothingClass)) tp2
    else {
      val t1 = mergeIfSuper(tp1, tp2, canConstrain)
      if (t1.exists) t1
      else {
        val t2 = mergeIfSuper(tp2, tp1, canConstrain)
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
    ((defn.NothingType: Type) /: tps)(lub(_,_, canConstrain = false))

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
  final def andType(tp1: Type, tp2: Type, erased: Boolean = ctx.erasedTypes) = ctx.traceIndented(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true) {
    val t1 = distributeAnd(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeAnd(tp2, tp1)
      if (t2.exists) t2
      else if (erased) erasedGlb(tp1, tp2, isJava = false)
      else liftIfHK(tp1, tp2, AndType(_, _), _ & _)
    }
  }

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
    if (tparams1.isEmpty)
      if (tparams2.isEmpty) op(tp1, tp2)
      else original(tp1, tp2.appliedTo(tp2.typeParams.map(_.paramInfoAsSeenFrom(tp2))))
    else if (tparams2.isEmpty)
      original(tp1.appliedTo(tp1.typeParams.map(_.paramInfoAsSeenFrom(tp1))), tp2)
    else
      HKTypeLambda(
        paramNames = (HKTypeLambda.syntheticParamNames(tparams1.length), tparams1, tparams2)
          .zipped.map((pname, tparam1, tparam2) =>
            pname.withVariance((tparam1.paramVariance + tparam2.paramVariance) / 2)))(
        paramInfosExp = tl => (tparams1, tparams2).zipped.map((tparam1, tparam2) =>
          tl.integrate(tparams1, tparam1.paramInfoAsSeenFrom(tp1)).bounds &
          tl.integrate(tparams2, tparam2.paramInfoAsSeenFrom(tp2)).bounds),
        resultTypeExp = tl =>
          original(tp1.appliedTo(tl.paramRefs), tp2.appliedTo(tl.paramRefs)))
  }

  /** Try to distribute `&` inside type, detect and handle conflicts
   *  @pre !(tp1 <: tp2) && !(tp2 <:< tp1) -- these cases were handled before
   */
  private def distributeAnd(tp1: Type, tp2: Type): Type = tp1 match {
    // opportunistically merge same-named refinements
    // this does not change anything semantically (i.e. merging or not merging
    // gives =:= types), but it keeps the type smaller.
    case tp1: RefinedType =>
      tp2 match {
        case tp2: RefinedType if tp1.refinedName == tp2.refinedName =>
          // Given two refinements `T1 { X = S1 }` and `T2 { X = S2 }` rewrite to
          // `T1 & T2 { X B }` where `B` is the conjunction of the bounds of `X` in `T1` and `T2`.
          //
          // However, if `homogenizeArgs` is set, and both aliases `X = Si` are
          // nonvariant, and `S1 =:= S2` (possibly by instantiating type parameters),
          // rewrite instead to `T1 & T2 { X = S1 }`. This rule is contentious because
          // it cuts the constraint set. On the other hand, without it we would replace
          // the two aliases by `T { X >: S1 | S2 <: S1 & S2 }`, which looks weird
          // and is probably not what's intended.
          val rinfo1 = tp1.refinedInfo
          val rinfo2 = tp2.refinedInfo
          val parent = tp1.parent & tp2.parent

          def isNonvariantAlias(tp: Type) = tp match {
            case tp: TypeAlias => tp.variance == 0
            case _ => false
          }
          if (homogenizeArgs &&
              isNonvariantAlias(rinfo1) && isNonvariantAlias(rinfo2))
            isSameType(rinfo1, rinfo2) // establish new constraint

          tp1.derivedRefinedType(parent, tp1.refinedName, rinfo1 & rinfo2)
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
          rt1 & tp2
      }
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying & tp2
    case tp1: AnnotatedType =>
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
      ExprType(rt1 | tp2.widenExpr)
    case tp1: TypeVar if tp1.isInstantiated =>
      tp1.underlying | tp2
    case tp1: AnnotatedType =>
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
  def copyIn(ctx: Context) = new TypeComparer(ctx)

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
  def showGoal(tp1: Type, tp2: Type)(implicit ctx: Context) = {
    println(ex"assertion failure for $tp1 <:< $tp2, frozen = $frozenConstraint")
    def explainPoly(tp: Type) = tp match {
      case tp: TypeParamRef => ctx.echo(s"TypeParamRef ${tp.show} found in ${tp.binder.show}")
      case tp: TypeRef if tp.symbol.exists => ctx.echo(s"typeref ${tp.show} found in ${tp.symbol.owner.show}")
      case tp: TypeVar => ctx.echo(s"typevar ${tp.show}, origin = ${tp.origin}")
      case _ => ctx.echo(s"${tp.show} is a ${tp.getClass}")
    }
    explainPoly(tp1)
    explainPoly(tp2)
  }

  /** Record statistics about the total number of subtype checks
   *  and the number of "successful" subtype checks, i.e. checks
   *  that form part of a subtype derivation tree that's ultimately successful.
   */
  def recordStatistics(result: Boolean, prevSuccessCount: Int) = {
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

  override def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean =
    traceIndented(s"hasMatchingMember(${show(tp1)} . $name, ${show(tp2.refinedInfo)}), member = ${show(tp1.member(name).info)}") {
      super.hasMatchingMember(name, tp1, tp2)
    }

  override def lub(tp1: Type, tp2: Type, canConstrain: Boolean = false) =
    traceIndented(s"lub(${show(tp1)}, ${show(tp2)}, canConstrain=$canConstrain)") {
      super.lub(tp1, tp2, canConstrain)
    }

  override def glb(tp1: Type, tp2: Type) =
    traceIndented(s"glb(${show(tp1)}, ${show(tp2)})") {
      super.glb(tp1, tp2)
    }

  override def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean): Boolean =
    traceIndented(i"add constraint $param ${if (fromBelow) ">:" else "<:"} $bound $frozenConstraint, constraint = ${ctx.typerState.constraint}") {
      super.addConstraint(param, bound, fromBelow)
    }

  override def copyIn(ctx: Context) = new ExplainingTypeComparer(ctx)

  override def compareHkApply2(tp1: Type, tp2: HKApply, tycon2: Type, args2: List[Type]): Boolean = {
    def addendum = ""
    traceIndented(i"compareHkApply2 $tp1, $tp2$addendum") {
      super.compareHkApply2(tp1, tp2, tycon2, args2)
    }
  }

  override def compareHkApply1(tp1: HKApply, tycon1: Type, args1: List[Type], tp2: Type): Boolean = {
    def addendum = ""
    traceIndented(i"compareHkApply1 $tp1, $tp2$addendum") {
      super.compareHkApply1(tp1, tycon1, args1, tp2)
    }
  }

  override def toString = "Subtype trace:" + { try b.toString finally b.clear() }
}
