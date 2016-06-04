package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Flags._, Names._, NameOps._, Denotations._
import Decorators._
import StdNames.{nme, tpnme}
import collection.mutable
import printing.Disambiguation.disambiguated
import util.{Stats, DotClass, SimpleMap}
import config.Config
import config.Printers._
import TypeErasure.{erasedLub, erasedGlb}
import TypeApplications._
import scala.util.control.NonFatal

/** Provides methods to compare types.
 */
class TypeComparer(initctx: Context) extends DotClass with ConstraintHandling {
  implicit val ctx: Context = initctx

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

  // Subtype testing `<:<`

  def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
    if (tp2 eq NoType) return false
    if ((tp2 eq tp1) || (tp2 eq WildcardType)) return true
    try isSubType(tp1, tp2)
    finally
      if (Config.checkConstraintsSatisfiable)
        assert(isSatisfiable, constraint.show)
  }

  protected def isSubType(tp1: Type, tp2: Type): Boolean = ctx.traceIndented(s"isSubType ${traceInfo(tp1, tp2)}", subtyping) /*<|<*/ {
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
        implicit val ctx: Context = this.ctx
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
              val sym1 = tp1.symbol
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
                compareHkApplyOBS(tp1, tp2, inOrder = true) ||
                compareHkApplyOBS(tp2, tp1, inOrder = false) ||
                thirdTryNamed(tp1, tp2)
            case _ =>
              compareHkApplyOBS(tp2, tp1, inOrder = false) ||
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
    case ErrorType =>
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
      compareHkApplyOBS(tp1, tp2, inOrder = true) ||
      thirdTry(tp1, tp2)
    case tp1: PolyParam =>
      def flagNothingBound = {
        if (!frozenConstraint && tp2.isRef(defn.NothingClass) && state.isGlobalCommittable) {
          def msg = s"!!! instantiated to Nothing: $tp1, constraint = ${constraint.show}"
          if (Config.failOnInstantiationToNothing) assert(false, msg)
          else ctx.log(msg)
        }
        true
      }
      def comparePolyParam =
        ctx.mode.is(Mode.TypevarsMissContext) ||
        isSubTypeWhenFrozen(bounds(tp1).hi, tp2) || {
          if (canConstrain(tp1)) addConstraint(tp1, tp2, fromBelow = false) && flagNothingBound
          else thirdTry(tp1, tp2)
        }
      comparePolyParam
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
    case OrType(tp11, tp12) =>
      isSubType(tp11, tp2) && isSubType(tp12, tp2)
    case ErrorType =>
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
            narrowGADTBounds(tp2, tp1, isUpper = false))
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
    case tp2: PolyParam =>
      def comparePolyParam =
        (ctx.mode is Mode.TypevarsMissContext) ||
        isSubTypeWhenFrozen(tp1, bounds(tp2).lo) || {
          if (canConstrain(tp2)) addConstraint(tp2, tp1.widenExpr, fromBelow = true)
          else fourthTry(tp1, tp2)
        }
      comparePolyParam
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
              compareHkApply(tp2, tp1, inOrder = false) ||
              compareHkLambda(tp2, tp1, inOrder = false) ||
              compareRefinedSlow ||
              fourthTry(tp1, tp2) ||
              compareAliasedRefined(tp2, tp1, inOrder = false)
          }
        else // fast path, in particular for refinements resulting from parameterization.
          isSubType(tp1, skipped2) &&
          isSubRefinements(tp1w.asInstanceOf[RefinedType], tp2, skipped2)
      }
      compareRefined
    case tp2: RecType =>
      val tp1stable = ensureStableSingleton(tp1)
      isSubType(fixRecs(tp1stable, tp1stable.widenExpr), tp2.substRecThis(tp2, tp1stable))
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
      eitherIsSubType(tp1, tp21, tp1, tp22) || fourthTry(tp1, tp2)
    case tp2 @ MethodType(_, formals2) =>
      def compareMethod = tp1 match {
        case tp1 @ MethodType(_, formals1) =>
          (tp1.signature sameParams tp2.signature) &&
            matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
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
               narrowGADTBounds(tp1, tp2, isUpper = true))
          }
          isSubType(hi1, tp2) || compareGADT
        case _ =>
          def isNullable(tp: Type): Boolean = tp.dealias match {
            case tp: TypeRef => tp.symbol.isNullableClass
            case tp: RefinedOrRecType => isNullable(tp.parent)
            case AndType(tp1, tp2) => isNullable(tp1) && isNullable(tp2)
            case OrType(tp1, tp2) => isNullable(tp1) || isNullable(tp2)
            case _ => false
          }
          (tp1.symbol eq NothingClass) && tp2.isInstanceOf[ValueType] ||
          (tp1.symbol eq NullClass) && isNullable(tp2)
      }
    case tp1: SingletonType =>
      /** if `tp2 == p.type` and `p: q.type` then try `tp1 <:< q.type` as a last effort.*/
      def comparePaths = tp2 match {
        case tp2: TermRef =>
          tp2.info match {
            case tp2i: TermRef =>
              isSubType(tp1, tp2i)
            case ExprType(tp2i: TermRef) if (ctx.phase.id > ctx.gettersPhase.id) =>
              // After getters, val x: T becomes def x: T
              isSubType(tp1, tp2i)
            case _ =>
              false
          }
        case _ =>
          false
      }
      isNewSubType(tp1.underlying.widenExpr, tp2) || comparePaths
    case tp1: RefinedType =>
      isNewSubType(tp1.parent, tp2) ||
      compareHkApply(tp1, tp2, inOrder = true) ||
      compareHkLambda(tp1, tp2, inOrder = true) ||
      compareAliasedRefined(tp1, tp2, inOrder = true)
    case tp1: RecType =>
      isNewSubType(tp1.parent, tp2)
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
      eitherIsSubType(tp11, tp2, tp12, tp2)
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

  /** Test whether `tp1` has a base type of the form `B[T1, ..., Tn]` where
   *   - `B` derives from one of the class symbols of `tp2`,
   *   - the type parameters of `B` match one-by-one the variances of `tparams`,
   *   - `B` satisfies predicate `p`.
   */
  private def testLifted(tp1: Type, tp2: Type, tparams: List[MemberBinding], p: Type => Boolean): Boolean = {
    val classBounds =
      if (Config.newHK) tp2.classSymbols
      else tp2.member(tpnme.hkApplyOBS).info.classSymbols
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

  /** If `projection` is a hk projection T#$apply with a constrainable poly param
   *  as type constructor and `other` is not a hk projection, then perform the following
   *  steps:
   *
   *   (1) If not `inOrder` then perform the next steps until they all succeed
   *       for each base type of other which
   *        - derives from a class bound of `projection`,
   *        - has the same number of type parameters than `projection`
   *        - has type parameter variances which conform to those of `projection`.
   *       If `inOrder` then perform the same steps on the original `other` type.
   *
   *   (2) Try to eta expand the constructor of `other`.
   *
   *   (3a) In mode `TypevarsMissConetxt` replace the projection's hk constructor parameter
   *        by the eta expansion of step (2) reapplied to the projection's arguments.
   *   (3b) In normal mode, try to unify the projection's hk constructor parameter with
   *        the eta expansion of step(2)
   *
   *   (4) If `inOrder`, test `projection <: other` else test `other <: projection`.
   */
  def compareHkApplyOBS(projection: NamedType, other: Type, inOrder: Boolean): Boolean = {
    def tryInfer(tp: Type): Boolean = ctx.traceIndented(i"compareHK($projection, $other, inOrder = $inOrder, constr = $tp)", subtyping) {
      tp match {
        case tp: TypeVar => tryInfer(tp.underlying)
        case param: PolyParam if canConstrain(param) =>

          def unifyWith(liftedOther: Type): Boolean = {
            subtyping.println(i"unify with $liftedOther")
            liftedOther.typeConstructor.widen match {
              case tycon: TypeRef if tycon.isEtaExpandable && tycon.typeParams.nonEmpty =>
                val (ok, projection1) =
                  if (ctx.mode.is(Mode.TypevarsMissContext))
                    (true, EtaExpansion(tycon).appliedTo(projection.argInfos))
                  else
                    (tryInstantiate(param, EtaExpansion(tycon)), projection)
                ok &&
                (if (inOrder) isSubType(projection1, other) else isSubType(other, projection1))
              case _ =>
                false
            }
          }
          val hkTypeParams = param.typeParams
          subtyping.println(i"classBounds = ${projection.prefix.member(tpnme.hkApplyOBS).info.classSymbols}")
          subtyping.println(i"base classes = ${other.baseClasses}")
          subtyping.println(i"type params = ${hkTypeParams.map(_.memberName)}")
          if (inOrder) unifyWith(other)
          else testLifted(other, projection.prefix, hkTypeParams, unifyWith)
        case _ =>
          false
      }
    }
    !Config.newHK && projection.name == tpnme.hkApplyOBS && !other.isHKApply &&
    tryInfer(projection.prefix.typeConstructor.dealias)
  }

  /** If `projection` is a hk projection T#$apply with a constrainable poly param
   *  as type constructor and `other` is not a hk projection, then perform the following
   *  steps:
   *
   *   (1) If not `inOrder` then perform the next steps until they all succeed
   *       for each base type of other which
   *        - derives from a class bound of `projection`,
   *        - has the same number of type parameters than `projection`
   *        - has type parameter variances which conform to those of `projection`.
   *       If `inOrder` then perform the same steps on the original `other` type.
   *
   *   (2) Try to eta expand the constructor of `other`.
   *
   *   (3a) In mode `TypevarsMissConetxt` replace the projection's hk constructor parameter
   *        by the eta expansion of step (2) reapplied to the projection's arguments.
   *   (3b) In normal mode, try to unify the projection's hk constructor parameter with
   *        the eta expansion of step(2)
   *
   *   (4) If `inOrder`, test `projection <: other` else test `other <: projection`.
   */
  def compareHkApply(app: RefinedType, other: Type, inOrder: Boolean): Boolean = {
    def tryInfer(tp: Type): Boolean = ctx.traceIndented(i"compareHK($app, $other, inOrder = $inOrder, constr = $tp)", subtyping) {
      tp match {
        case tp: TypeVar => tryInfer(tp.underlying)
        case param: PolyParam if canConstrain(param) =>

          def unifyWith(liftedOther: Type): Boolean = {
            subtyping.println(i"unify with $liftedOther")
            liftedOther.typeConstructor.widen match {
              case tycon: TypeRef if tycon.isEtaExpandable && tycon.typeParams.nonEmpty =>
                val (ok, app1) =
                  if (ctx.mode.is(Mode.TypevarsMissContext))
                    (true, EtaExpansion(tycon).appliedTo(app.argInfos))
                  else
                    (tryInstantiate(param, EtaExpansion(tycon)), app)
                ok &&
                (if (inOrder) isSubType(app1, other) else isSubType(other, app1))
              case _ =>
                false
            }
          }
          val hkTypeParams = param.typeParams
          subtyping.println(i"classBounds = ${app.classSymbols}")
          subtyping.println(i"base classes = ${other.baseClasses}")
          subtyping.println(i"type params = $hkTypeParams")
          if (inOrder) unifyWith(other)
          else testLifted(other, app, hkTypeParams, unifyWith)
        case _ =>
          false
      }
    }
    Config.newHK && app.isHKApply && !other.isHKApply && {
      val reduced = app.normalizeHkApply
      if (reduced ne app)
        if (inOrder) isSubType(reduced, other) else isSubType(other, reduced)
      else tryInfer(app.typeConstructor.dealias)
    }
  }

  /** Compare type lambda with non-lambda type. */
  def compareHkLambda(rt: RefinedType, other: Type, inOrder: Boolean) = rt match {
    case TypeLambda(args, body) =>
      args.length == other.typeParams.length && {
        val applied = other.appliedTo(argRefs(rt, args.length))
        if (inOrder) isSubType(body, applied)
        else body match {
          case body: TypeBounds => body.contains(applied) // Can be dropped?
          case _ => isSubType(applied, body)
        }
      }
    case _ =>
      false
  }

  /** Say we are comparing a refined type `P{type M = U}` or `P{type M >: L <: U}`.
   *  If P#M refers to a BaseTypeArg aliased to some other typeref P#N,
   *  do the same comparison with `P{type N = U}` or `P{type N >: L <: U}`, respectively.
   *  This allows to handle situations involving named type params like this one:
   *
   *      trait Lambda[type Elem]
   *      trait Lst[T] extends Lambda[T]
   *
   *  compareAliasedRefined is necessary so we establish that
   *
   *      Lst[Int] = Lst[Elem = Int]
   */
  private def compareAliasedRefined(rt: RefinedType, other: Type, inOrder: Boolean) = {
    val mbr = refinedSymbol(rt)
    mbr.is(BaseTypeArg) && {
      mbr.info match {
        case TypeAlias(TypeRef(_, aliasName)) =>
          val rt1 = rt.derivedRefinedType(rt.parent, aliasName, rt.refinedInfo)
          subtyping.println(i"rewiring $rt to $rt1 in comparison with $other")
          if (inOrder) isSubType(rt1, other) else isSubType(other, rt1)
        case _ =>
          false
      }
    }
  }

  /** Replace any top-level recursive type `{ z => T }` in `tp` with
   *  `[z := anchor]T`.
   */
  private def fixRecs(anchor: SingletonType, tp: Type): Type = {
    def fix(tp: Type): Type = tp.stripTypeVar match {
      case tp: RecType => fix(tp.parent).substRecThis(tp, anchor)
      case tp @ RefinedType(parent, rname, rinfo) => tp.derivedRefinedType(fix(parent), rname, rinfo)
      case tp: PolyParam => fixOrElse(bounds(tp).hi, tp)
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

  /** The symbol referred to in the refinement of `rt` */
  private def refinedSymbol(rt: RefinedType) = rt.parent.member(rt.refinedName).symbol

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
  private def eitherIsSubType(tp11: Type, tp21: Type, tp12: Type, tp22: Type) = {
    val preConstraint = constraint
    isSubType(tp11, tp21) && {
      val leftConstraint = constraint
      constraint = preConstraint
      if (!(isSubType(tp12, tp22) && subsumes(leftConstraint, constraint, preConstraint)))
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
    } else isSubType(tp1, tp2)

  /** Does type `tp1` have a member with name `name` whose normalized type is a subtype of
   *  the normalized type of the refinement `tp2`?
   *  Normalization is as follows: If `tp2` contains a skolem to its refinement type,
   *  rebase both itself and the member info of `tp` on a freshly created skolem type.
   */
  protected def hasMatchingMember(name: Name, tp1: Type, tp2: RefinedType): Boolean = {
    val rebindNeeded = tp2.refinementRefersToThis
    val base = if (rebindNeeded) ensureStableSingleton(tp1) else tp1
    val rinfo2 = if (rebindNeeded) tp2.refinedInfo.substRefinedThis(tp2, base) else tp2.refinedInfo
    val mbr = base.member(name)

    def qualifies(m: SingleDenotation) = isSubType(m.info, rinfo2)

    def memberMatches: Boolean = mbr match { // inlined hasAltWith for performance
      case mbr: SingleDenotation => qualifies(mbr)
      case _ => mbr hasAltWith qualifies
    }

    // special case for situations like:
    //    class C { type T }
    //    val foo: C
    //    foo.type <: C { type T = foo.T }
    def selfReferentialMatch = tp1.isInstanceOf[SingletonType] && {
      rinfo2 match {
        case rinfo2: TypeAlias =>
          !defn.isBottomType(base.widen) && (base select name) =:= rinfo2.alias
        case _ => false
      }
    }

    def varianceMatches = true // TODO: fill in

    /*>|>*/ ctx.traceIndented(i"hasMatchingMember($base . $name :? ${tp2.refinedInfo}) ${mbr.info.show} $rinfo2", subtyping) /*<|<*/ {
      (memberMatches || selfReferentialMatch) && varianceMatches
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
    case tp1 @ RefinedType(parent1, name1, rinfo1: TypeAlias)
    if name1 == tp2.refinedName &&
       !tp2.refinementRefersToThis &&
       !tp1.refinementRefersToThis =>
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
    case tp: RefinedType => isCovered(tp.parent) && !refinedSymbol(tp).is(BaseTypeArg)
    case tp: RecType => isCovered(tp.parent)
    case tp: AnnotatedType => isCovered(tp.underlying)
    case AndType(tp1, tp2) => isCovered(tp1) && isCovered(tp2)
    case _ => false
  }

  /** Defer constraining type variables when compared against prototypes */
  def isMatchedByProto(proto: ProtoType, tp: Type) = tp.stripTypeVar match {
    case tp: PolyParam if constraint contains tp => true
    case _ => proto.isMatchedBy(tp)
  }

  /** Can type `tp` be constrained from above by adding a constraint to
   *  a typevar that it refers to? In that case we have to be careful not
   *  to approximate with the lower bound of a type in `thirdTry`. Instead,
   *  we should first unroll `tp1` until we hit the type variable and bind the
   *  type variable with (the corresponding type in) `tp2` instead.
   */
  private def isCappable(tp: Type): Boolean = tp match {
    case tp: PolyParam => constraint contains tp
    case tp: TypeProxy => isCappable(tp.underlying)
    case tp: AndOrType => isCappable(tp.tp1) || isCappable(tp.tp2)
    case _ => false
  }

  /** Narrow gadt.bounds for the type parameter referenced by `tr` to include
   *  `bound` as an upper or lower bound (which depends on `isUpper`).
   *  Test that the resulting bounds are still satisfiable.
   */
  private def narrowGADTBounds(tr: NamedType, bound: Type, isUpper: Boolean): Boolean =
    ctx.mode.is(Mode.GADTflexible) && {
    val tparam = tr.symbol
    typr.println(s"narrow gadt bound of $tparam: ${tparam.info} from ${if (isUpper) "above" else "below"} to $bound ${bound.isRef(tparam)}")
    !bound.isRef(tparam) && {
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
          tp1.isImplicit == tp2.isImplicit &&
            matchingParams(tp1.paramTypes, tp2.paramTypes, tp1.isJava, tp2.isJava) &&
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

  /** Are `syms1` and `syms2` parameter lists with pairwise equivalent types? */
  private def matchingParams(formals1: List[Type], formals2: List[Type], isJava1: Boolean, isJava2: Boolean): Boolean = formals1 match {
    case formal1 :: rest1 =>
      formals2 match {
        case formal2 :: rest2 =>
          (isSameTypeWhenFrozen(formal1, formal2)
            || isJava1 && (formal2 isRef ObjectClass) && (formal1 isRef AnyClass)
            || isJava2 && (formal1 isRef ObjectClass) && (formal2 isRef AnyClass)) &&
          matchingParams(rest1, rest2, isJava1, isJava2)
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
    ((defn.NothingType: Type) /: tps)(lub)

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
   *  In these cases, a MergeError is thrown.
   */
  final def andType(tp1: Type, tp2: Type, erased: Boolean = ctx.erasedTypes) = ctx.traceIndented(s"glb(${tp1.show}, ${tp2.show})", subtyping, show = true) {
    val t1 = distributeAnd(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeAnd(tp2, tp1)
      if (t2.exists) t2
      else if (erased) erasedGlb(tp1, tp2, isJava = false)
      else liftIfHK(tp1, tp2, AndType(_, _))
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
      else liftIfHK(tp1, tp2, OrType(_, _))
    }
  }

  /** `op(tp1, tp2)` unless `tp1` and `tp2` are type-constructors with at least
   *  some unnamed type parameters.
   *  In the latter case, combine `tp1` and `tp2` under a type lambda like this:
   *
   *    [X1, ..., Xn] -> op(tp1[X1, ..., Xn], tp2[X1, ..., Xn])
   *
   *  Note: There is a tension between named and positional parameters here, which
   *  is impossible to resolve completely. Say you have
   *
   *    C[type T], D[type U]
   *
   *  Then do you expand `C & D` to `[T] -> C[T] & D[T]` or not? Under the named
   *  type parameter interpretation, this would be wrong whereas under the traditional
   *  higher-kinded interpretation this would be required. The problem arises from
   *  allowing both interpretations. A possible remedy is to be somehow stricter
   *  in where we allow which interpretation.
   */
  private def liftIfHK(tp1: Type, tp2: Type, op: (Type, Type) => Type) =
    if (Config.newHK) {
      val tparams1 = tp1.typeParams
      val tparams2 = tp2.typeParams
      if (tparams1.isEmpty || tparams2.isEmpty) op(tp1, tp2)
      else if (tparams1.length != tparams2.length) mergeConflict(tp1, tp2)
      else {
        val bindings: List[RecType => TypeBounds] =
          (tparams1, tparams2).zipped.map { (tparam1, tparam2) =>
            val b1: RecType => TypeBounds =
              tparam1.memberBoundsAsSeenFrom(tp1).recursify(tparams1)
            val b2: RecType => TypeBounds =
              tparam2.memberBoundsAsSeenFrom(tp2).recursify(tparams2)
            (rt: RecType) => (b1(rt) & b2(rt))
              .withBindingKind(
                BindingKind.fromVariance(
                  (tparam1.memberVariance + tparam2.memberVariance) / 2))
          }
        val app1: RecType => Type = rt => tp1.appliedTo(argRefs(rt, tparams1.length))
        val app2: RecType => Type = rt => tp2.appliedTo(argRefs(rt, tparams2.length))
        val body: RecType => Type = rt => op(app1(rt), app2(rt))
        TypeLambda(bindings, body)
      }
    }
    else {
      val tparams1 = tp1.typeParamSymbols
      val tparams2 = tp2.typeParamSymbols
      def onlyNamed(tparams: List[TypeSymbol]) = tparams.forall(!_.is(ExpandedName))
      if (tparams1.isEmpty || tparams2.isEmpty ||
        onlyNamed(tparams1) && onlyNamed(tparams2)) op(tp1, tp2)
      else if (tparams1.length != tparams2.length) mergeConflict(tp1, tp2)
      else hkCombineOBS(tp1, tp2, tparams1, tparams2, op)
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
          tp1.derivedRefinedType(
              tp1.parent & tp2.parent,
              tp1.refinedName,
              tp1.refinedInfo & tp2.refinedInfo.substRefinedThis(tp2, RefinedThis(tp1)))
        case _ =>
          NoType
      }
    case tp1: RecType =>
      tp1.rebind(distributeAnd(tp1.parent, tp2))
    case tp1: TypeBounds =>
      tp2 match {
        case tp2: TypeBounds => tp1 & tp2
        case tp2: ClassInfo if tp1 contains tp2 => tp2
        case _ => mergeConflict(tp1, tp2)
      }
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo if tp1.cls eq tp2.cls => tp1.derivedClassInfo(tp1.prefix & tp2.prefix)
        case tp2: TypeBounds if tp2 contains tp1 => tp1
        case _ => mergeConflict(tp1, tp2)
      }
    case tp1 @ MethodType(names1, formals1) =>
      tp2 match {
        case tp2 @ MethodType(names2, formals2)
        if matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
           tp1.isImplicit == tp2.isImplicit =>
          tp1.derivedMethodType(
              mergeNames(names1, names2, nme.syntheticParamName),
              formals1, tp1.resultType & tp2.resultType.subst(tp2, tp1))
        case _ =>
          mergeConflict(tp1, tp2)
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType if matchingTypeParams(tp1, tp2) =>
          tp1.derivedPolyType(
              mergeNames(tp1.paramNames, tp2.paramNames, tpnme.syntheticTypeParamName),
              tp1.paramBounds, tp1.resultType & tp2.resultType.subst(tp2, tp1))
        case _ =>
          mergeConflict(tp1, tp2)
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

  /** Try to distribute `|` inside type, detect and handle conflicts
   *  Note that, unlike for `&`, a disjunction cannot be pushed into
   *  a refined or applied type. Example:
   *
   *     List[T] | List[U] is not the same as List[T | U].
   *
   *  The rhs is a proper supertype of the lhs.
   */
  private def distributeOr(tp1: Type, tp2: Type): Type = tp1 match {
    case tp1: TypeBounds =>
      tp2 match {
        case tp2: TypeBounds => tp1 | tp2
        case tp2: ClassInfo if tp1 contains tp2 => tp1
        case _ => mergeConflict(tp1, tp2)
      }
    case tp1: ClassInfo =>
      tp2 match {
        case tp2: ClassInfo if tp1.cls eq tp2.cls => tp1.derivedClassInfo(tp1.prefix | tp2.prefix)
        case tp2: TypeBounds if tp2 contains tp1 => tp2
        case _ => mergeConflict(tp1, tp2)
      }
    case tp1 @ MethodType(names1, formals1) =>
      tp2 match {
        case tp2 @ MethodType(names2, formals2)
        if matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
           tp1.isImplicit == tp2.isImplicit =>
          tp1.derivedMethodType(
              mergeNames(names1, names2, nme.syntheticParamName),
              formals1, tp1.resultType | tp2.resultType.subst(tp2, tp1))
        case _ =>
          mergeConflict(tp1, tp2)
      }
    case tp1: PolyType =>
      tp2 match {
        case tp2: PolyType if matchingTypeParams(tp1, tp2) =>
          tp1.derivedPolyType(
              mergeNames(tp1.paramNames, tp2.paramNames, tpnme.syntheticTypeParamName),
              tp1.paramBounds, tp1.resultType | tp2.resultType.subst(tp2, tp1))
        case _ =>
          mergeConflict(tp1, tp2)
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

  /** Handle merge conflict by throwing a `MergeError` exception */
  private def mergeConflict(tp1: Type, tp2: Type): Type = {
    def showType(tp: Type) = tp match {
      case ClassInfo(_, cls, _, _, _) => cls.showLocated
      case bounds: TypeBounds => i"type bounds $bounds"
      case _ => tp.show
    }
    throw new MergeError(s"cannot merge ${showType(tp1)} with ${showType(tp2)}", tp1, tp2)
  }

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
          asGoodParams(tp1.paramTypes, tp2.paramTypes) &&
          (!asGoodParams(tp2.paramTypes, tp1.paramTypes) ||
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
    println(disambiguated(implicit ctx => s"assertion failure for ${tp1.show} <:< ${tp2.show}, frozen = $frozenConstraint"))
    def explainPoly(tp: Type) = tp match {
      case tp: PolyParam => ctx.echo(s"polyparam ${tp.show} found in ${tp.binder.show}")
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

  override def compareHkApply(app: RefinedType, other: Type, inOrder: Boolean) =
    if (app.isHKApply)
      traceIndented(i"compareHkApply $app, $other, $inOrder, ${app.normalizeHkApply}") {
        super.compareHkApply(app, other, inOrder)
      }
    else super.compareHkApply(app, other, inOrder)

  override def compareHkLambda(rt: RefinedType, other: Type, inOrder: Boolean) =
    if (!Config.newHK && rt.refinedName == tpnme.hkApplyOBS ||
        Config.newHK && rt.isTypeParam)
      traceIndented(i"compareHkLambda $rt, $other, $inOrder") {
        super.compareHkLambda(rt, other, inOrder)
      }
    else super.compareHkLambda(rt, other, inOrder)

  override def toString = "Subtype trace:" + { try b.toString finally b.clear() }
}
