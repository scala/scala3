package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Flags._, Names._, NameOps._
import Decorators.sourcePos
import StdNames.{nme, tpnme}
import collection.mutable
import printing.Disambiguation.disambiguated
import util.SimpleMap

/** Provides methods to compare types.
 *  @param  constraint The initial constraint which is assumed to hold for the comparisons.
 *                      The constraint set is updated when undetermined type parameters
 *                      in the constraint's domain are compared.
 */
class TypeComparer(initctx: Context) extends DotClass {
  implicit val ctx = initctx

  val state = ctx.typerState
  import state.constraint

  private var pendingSubTypes: mutable.Set[(Type, Type)] = null
  private var recCount = 0

  protected var frozenConstraint = false

  private var myAnyClass: ClassSymbol = null
  private var myNothingClass: ClassSymbol = null
  private var myNullClass: ClassSymbol = null
  private var myObjectClass: ClassSymbol = null
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

  /** Add the constraint `<bounds.lo <: param <: bounds.hi>`
   *  to `constraint`.
   *  @pre `param` is in the constraint's domain
   */
  def addConstraint1(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean = {
      val pt = param.binder
      val pnum = param.paramNum
      val oldEntries = constraint(pt)
      val oldBounds = oldEntries(pnum).asInstanceOf[TypeBounds]
      val constrBounds = if (fromBelow) TypeBounds.lower(bound) else TypeBounds.upper(bound)
      val newBounds = oldBounds & constrBounds
      if (oldBounds ne newBounds) {
        val newEntries = oldEntries.clone
        newEntries(pnum) = newBounds
        constraint = constraint.updated(pt, newEntries)
      }
      isSubType(newBounds.lo, newBounds.hi)
    }

  def addConstraint(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean =
    !frozenConstraint && {
      bound match {
        case bound: TypeVar =>
          if (bound.isInstantiated)
            addConstraint1(param, bound.instanceOpt, fromBelow)
          else
            addConstraint1(param, bound, fromBelow) &&
            addConstraint1(bound.origin, param, !fromBelow)
        case _ =>
          addConstraint1(param, bound, fromBelow)
      }
    }

  /** Solve constraint for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound. However, any occurrences
   *  of the parameter in a refinement somewhere in the bound are removed.
   *  (Such occurrences can arise for F-bounded types).
   *  The type parameter is removed from the constraint's domain and all its
   *  occurrences are replaced by its approximation.
   *  @return the instantiating type
   *  @pre `param` is associated with type bounds in the current constraint.
   */
  def approximate(param: PolyParam, fromBelow: Boolean): Type = {
    val avoidParam = new TypeMap {
      override def apply(tp: Type) = mapOver {
        tp match {
          case tp: RefinedType if param occursIn tp.refinedInfo => tp.parent
          case _ => tp
        }
      }
    }
    val bounds = constraint(param).asInstanceOf[TypeBounds]
    val bound = if (fromBelow) bounds.lo else bounds.hi
    val inst = avoidParam(bound)
    println(s"approx ${param.show}, from below = $fromBelow, bound = ${bound.show}, inst = ${inst.show}")
    constraint = constraint.replace(param, inst)
    inst
  }

  def isSubTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    frozenConstraint = true
    try isSubType(tp1, tp2)
    finally frozenConstraint = false
  }

  def isSubType(tp1: Type, tp2: Type): Boolean =
    if (tp1 == NoType || tp2 == NoType) false
    else if (tp1 eq tp2) true
    else {
      val cs = constraint
      try {
        recCount += 1
/* !!! DEBUG
        if (isWatched(tp1) && isWatched(tp2) && !(this.isInstanceOf[ExplainingTypeComparer])) {
          val explained = new ExplainingTypeComparer(ctx)
          println("***** watched:")
          println(TypeComparer.explained(_.typeComparer.isSubType(tp1, tp2)))
        }
*/
        val result =
          if (recCount < LogPendingSubTypesThreshold) firstTry(tp1, tp2)
          else monitoredIsSubType(tp1, tp2)
        recCount -= 1
        if (!result) constraint = cs
        result
      } catch {
        case ex: Throwable =>
          recCount -= 1
          constraint = cs
          throw ex
      }
    }

  def monitoredIsSubType(tp1: Type, tp2: Type) = {
    if (pendingSubTypes == null) {
      pendingSubTypes = new mutable.HashSet[(Type, Type)]
      ctx.log(s"!!! deep subtype recursion involving $tp1 <:< $tp2")
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

  def firstTry(tp1: Type, tp2: Type): Boolean = ctx.debugTraceIndented(s"$tp1 <:< $tp2") {
    tp2 match {
      case tp2: NamedType =>
        tp1 match {
          case tp1: NamedType =>
            val sym1 = tp1.symbol
            val sym2 = tp2.symbol
            val pre1 = tp1.prefix
            val pre2 = tp2.prefix
            if (sym1 == sym2) (
              ctx.erasedTypes
              || sym1.isStaticOwner
              || isSubType(pre1, pre2))
            else (
              tp1.name == tp2.name && isSubType(pre1, pre2)
              || sym2.isClass && {
                val base = tp1.baseType(sym2)
                base.exists && (base ne tp1) && isSubType(base, tp2)
              }
              || thirdTryNamed(tp1, tp2))
          case _ =>
            secondTry(tp1, tp2)
        }
      case tp2 @ ThisType(cls) =>
        if (cls is ModuleClass)
          tp1 match {
            case tp1: TermRef =>
              return tp1.symbol.moduleClass == cls && tp1.prefix <:< cls.owner.thisType
            case _ =>
          }
        secondTry(tp1, tp2)
      case tp2: PolyParam =>
        tp2 == tp1 || {
          //println(constraint.show)
          constraint(tp2) match {
            case TypeBounds(lo, _) => isSubType(tp1, lo) || addConstraint(tp2, tp1.widen, fromBelow = true)
            case _ => secondTry(tp1, tp2)
          }
        }
      case tp2: BoundType =>
        tp2 == tp1 || secondTry(tp1, tp2)
      case tp2: TypeVar =>
        isSubType(tp1, tp2.underlying)
      case tp2: ProtoType =>
        tp2.isMatchedBy(tp1)
      case tp2: WildcardType =>
        tp2.optBounds match {
          case TypeBounds(_, hi) => isSubType(tp1, hi)
          case NoType => true
        }
      case tp2: AnnotatedType =>
        isSubType(tp1, tp2.tpe) // todo: refine?
      case ErrorType =>
        true
      case _ =>
        secondTry(tp1, tp2)
    }
  }

  def secondTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1 @ ThisType(cls) =>
      if (cls is ModuleClass)
        tp2 match {
          case tp2: TermRef =>
            return tp2.symbol.moduleClass == cls && cls.owner.thisType <:< tp2.prefix
          case _ =>
        }
      thirdTry(tp1, tp2)
    case tp1: PolyParam =>
      (tp1 == tp2) || {
        constraint(tp1) match {
          case TypeBounds(_, hi) => isSubType(hi, tp2) || addConstraint(tp1, tp2, fromBelow = false)
          case _ => thirdTry(tp1, tp2)
        }
      }
    case tp1: BoundType =>
      tp1 == tp2 || secondTry(tp1, tp2)
    case tp1: TypeVar =>
      isSubType(tp1.underlying, tp2)
    case tp1: WildcardType =>
      tp1.optBounds match {
        case TypeBounds(lo, _) => isSubType(lo, tp2)
        case _ => true
      }
    case tp1: AnnotatedType =>
      isSubType(tp1.tpe, tp2)
    case ErrorType =>
      true
    case _ =>
      thirdTry(tp1, tp2)
  }

  def thirdTryNamed(tp1: Type, tp2: NamedType): Boolean = tp2.info match {
    case TypeBounds(lo2, hi2) =>
      (isSubType(tp1, lo2)
        || (tp2.symbol is GADTFlexType) && trySetType(tp2, TypeBounds(lo2 | tp1, hi2))
        || fourthTry(tp1, tp2))
    case _ =>
      val cls2 = tp2.symbol
      (cls2 == defn.SingletonClass && tp1.isStable
        || cls2 == defn.NotNullClass && tp1.isNotNull
        || (defn.hkTraits contains cls2) && isSubTypeHK(tp1, tp2)
        || fourthTry(tp1, tp2))
  }

  def thirdTry(tp1: Type, tp2: Type): Boolean = tp2 match {
    case tp2: NamedType =>
      thirdTryNamed(tp1, tp2)
    case tp2 @ RefinedType(parent2, name2) =>
      tp1 match {
        case tp1 @ RefinedType(parent1, name1) if (name1 == name2) && name1.isTypeName =>
          // optimized case; all info on t1.name2 is in refinement tp1.refinedInfo.
          isSubType(tp1, parent2) && isSubType(tp1.refinedInfo, tp2.refinedInfo)
        case _ =>
          def hasMatchingMember(name: Name): Boolean = traceIndented(s"hasMatchingMember($name)") {
            tp1.member(name).hasAltWith(alt => isSubType(alt.info, tp2.refinedInfo)) ||
            name.isHkParamName && {
              val idx = name.hkParamIndex
              val tparams = tp1.typeParams
              idx < tparams.length && hasMatchingMember(tparams(idx).name)
            }
          }
          isSubType(tp1, parent2) && (
               name2 == nme.WILDCARD
            || hasMatchingMember(name2)
            || fourthTry(tp1, tp2))
      }
    case AndType(tp21, tp22) =>
      isSubType(tp1, tp21) && isSubType(tp1, tp22)
    case OrType(tp21, tp22) =>
      isSubType(tp1, tp21) || isSubType(tp1, tp22)
    case tp2 @ MethodType(_, formals1) =>
      tp1 match {
        case tp1 @ MethodType(_, formals2) =>
          tp1.signature == tp2.signature &&
            matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
            tp1.isImplicit == tp2.isImplicit && // needed?
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
    case tp2: PolyType =>
      tp1 match {
        case tp1: PolyType =>
          tp1.signature == tp2.signature &&
          matchingTypeParams(tp1, tp2) &&
          isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
    case tp2 @ ExprType(restpe2) =>
      tp1 match {
        case tp1 @ ExprType(restpe1) =>
          isSubType(restpe1, restpe2)
        case _ =>
          isSubType(tp1, restpe2)
      }
    case tp2 @ TypeBounds(lo2, hi2) =>
      tp1 match {
        case tp1 @ TypeBounds(lo1, hi1) =>
          val v = tp1.variance + tp2.variance
          ((v > 0) || (lo2 isRef NothingClass) || isSubType(lo2, lo1)) &&
          ((v < 0) || (hi2 isRef AnyClass) || isSubType(hi1, hi2))
        case tp1: ClassInfo =>
          val tt = tp1.typeConstructor // was typeTemplate
          isSubType(lo2, tt) && isSubType(tt, hi2)
        case _ =>
          false
      }
    case ClassInfo(pre2, cls2, _, _, _) =>
      tp1 match {
        case ClassInfo(pre1, cls1, _, _, _) =>
          (cls1 eq cls2) && isSubType(pre2, pre1)
        case _ =>
          false
      }
    case _ =>
      fourthTry(tp1, tp2)
  }

  def fourthTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: TypeRef =>
      ((tp1.symbol eq NothingClass)
        || (tp1.symbol eq NullClass) && tp2.dealiasedTypeSymbol.isNonValueClass
        || (tp1.info match {
              case TypeBounds(lo1, hi1) =>
                isSubType(hi1, tp2) ||
                (tp1.symbol is GADTFlexType) && trySetType(tp1, TypeBounds(lo1, hi1 & tp2))
              case _ => false
           }))
    case tp1: SingletonType =>
      val underlying = tp1.underlying match {
        case underlying: ExprType => underlying.resultType
        case underlying => underlying
      }
      isSubType(underlying, tp2)
    case tp1: RefinedType =>
      isSubType(tp1.parent, tp2)
    case AndType(tp11, tp12) =>
      isSubType(tp11, tp2) || isSubType(tp12, tp2)
    case OrType(tp11, tp12) =>
      isSubType(tp11, tp2) && isSubType(tp12, tp2)
    case _ =>
      false
  }
  /* not needed
    def isSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[TypeSymbol]): Boolean = tparams match {
      case tparam :: tparams1 =>
        val variance = tparam.variance
        val t1 = tps1.head
        val t2 = tps2.head
        (variance > 0 || isSubType(t2, t1)) &&
        (variance < 0 || isSubType(t1, t2)) &&
        isSubArgs(tps1.tail, tps2.tail, tparams1)
      case _ =>
        assert(tps1.isEmpty && tps2.isEmpty)
        true
    }
*/
  /** Is `tp1` a subtype of a type `tp2` of the form
   *  `scala.HigerKindedXYZ { ... }?
   *  This is the case if `tp1` and `tp2` have the same number
   *  of type parameters, the bounds of tp1's paremeters
   *  are contained in the corresponding bounds of tp2's parameters
   *  and the variances of correesponding parameters agree.
   */
  def isSubTypeHK(tp1: Type, tp2: Type): Boolean = {
    val tparams = tp1.typeParams
    val hkArgs = tp2.typeArgs
    (hkArgs.length == tparams.length) && {
      val base = ctx.newSkolemSingleton(tp1)
      (tparams, hkArgs).zipped.forall { (tparam, hkArg) =>
        base.memberInfo(tparam) <:< hkArg.bounds // TODO: base.memberInfo needed?
      } &&
        (tparams, tp2.typeSymbol.typeParams).zipped.forall { (tparam, tparam2) =>
          tparam.variance == tparam2.variance
        }
    }
  }

  def trySetType(tr: NamedType, bounds: TypeBounds): Boolean =
    (bounds.lo <:< bounds.hi) &&
    { tr.symbol.changeGADTInfo(bounds); true }

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

  /** Do poly types `poly1` and `poly2` have type parameters that
   *  have the same bounds (after renaming one set to the other)?
   */
  private def matchingTypeParams(poly1: PolyType, poly2: PolyType): Boolean =
    (poly1.paramBounds corresponds poly2.paramBounds)((b1, b2) =>
      isSameType(b1, b2.subst(poly2, poly1)))

  /** Two types are the same if are mutual subtypes of each other */
  def isSameType(tp1: Type, tp2: Type): Boolean =
    if (tp1 == NoType || tp2 == NoType) false
    else if (tp1 eq tp2) true
    else isSubType(tp1, tp2) && isSubType(tp2, tp1)

  /** The greatest lower bound of two types */
  def glb(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else if (!tp1.exists || (tp1 isRef AnyClass) || (tp2 isRef NothingClass)) tp2
    else if (!tp2.exists || (tp2 isRef AnyClass) || (tp1 isRef NothingClass)) tp1
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

  /** The greatest lower bound of a list types */
  final def glb(tps: List[Type]): Type =
    (defn.AnyType /: tps)(glb)

  /** The least upper bound of two types */
  def lub(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else if (!tp1.exists || (tp1 isRef AnyClass) || (tp2 isRef NothingClass)) tp1
    else if (!tp2.exists || (tp2 isRef AnyClass) || (tp1 isRef NothingClass)) tp2
    else {
      val t1 = mergeIfSuper(tp1, tp2)
      if (t1.exists) t1
      else {
        val t2 = mergeIfSuper(tp2, tp1)
        if (t2.exists) t2
        else orType(tp1, tp2)
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
   *  unfeasible TypeBounds (where low bound is not a subtype of high bound).
   *  Such TypeBounds can also be arbitrarily instantiated. In both cases we need to
   *  make sure that such types do not actually arise in source programs.
   */
  final def andType(tp1: Type, tp2: Type) = ctx.traceIndented(s"glb(${tp1.show}, ${tp2.show})", show = true) {
    val t1 = distributeAnd(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeAnd(tp2, tp1)
      if (t2.exists) t2
      else AndType(tp1, tp2)
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
   */
  final def orType(tp1: Type, tp2: Type) = {
    val t1 = distributeOr(tp1, tp2)
    if (t1.exists) t1
    else {
      val t2 = distributeOr(tp2, tp1)
      if (t2.exists) t2
      else OrType(tp1, tp2)
    }
  }

  /** Try to distribute `&` inside type, detect and handle conflicts */
  private def distributeAnd(tp1: Type, tp2: Type): Type = tp1 match {
    case tp1 @ TypeBounds(lo1, hi1) =>
      tp2 match {
        case tp2 @ TypeBounds(lo2, hi2) =>
          if ((lo1 eq hi1) && (lo2 eq hi2)) {
            val v = (tp1.variance + tp2.variance) / 2
            if (v > 0) return TypeAlias(hi1 & hi2, v)
            if (v < 0) return TypeAlias(lo1 | lo2, v)
          }
          TypeBounds(lo1 | lo2, hi1 & hi2)
        case _ =>
          andConflict(tp1, tp2)
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
    case tp1: RefinedType =>
      // opportunistically merge same-named refinements
      // this does not change anything semantically (i.e. merging or not merging
      // gives =:= types), but it keeps the type smaller.
      tp2 match {
        case tp2: RefinedType if tp1.refinedName == tp2.refinedName =>
          tp1.derivedRefinedType(
              tp1.parent & tp2.parent, tp1.refinedName,
              tp1.refinedInfo & tp2.refinedInfo)
        case _ =>
          NoType
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
    case tp1 @ TypeBounds(lo1, hi1) =>
      tp2 match {
        case tp2 @ TypeBounds(lo2, hi2) =>
          if ((lo1 eq hi1) && (lo2 eq hi2)) {
            val v = (tp1.variance + tp2.variance) / 2
            if (v > 0) return TypeAlias(hi1 | hi2, v)
            if (v < 0) return TypeAlias(lo1 & lo2, v)
          }
          TypeBounds(lo1 & lo2, hi1 | hi2)
        case _ =>
          orConflict(tp1, tp2)
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
      tp2 match {
        case ExprType(rt2) =>
          ExprType(rt1 | rt2)
        case _ =>
          ExprType(rt1 | tp2)
      }
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
          (tp1.prefix <:< tp2.prefix) || (tp1.cls.owner derivesFrom tp2.cls.owner)
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
            (formals2 corresponds formals1)(_ <:< _)
          asGoodParams(tp1.paramTypes, tp2.paramTypes) &&
          (!asGoodParams(tp2.paramTypes, tp1.paramTypes) ||
           isAsGood(tp1.resultType, tp2.resultType))
        case _ =>
          false
      }
    case _ =>
      false
  }
/*
  def widenInferred(tp: Type) = tp match {
    case tp: OrType =>
      val alts = tp.mapReduceOr(_ :: Nil)(_ ::: _)

  }
*/
  def copyIn(ctx: Context) = new TypeComparer(ctx)

  def traceIndented[T](str: String)(op: => T): T = op
}

object TypeComparer {
  def explained[T](op: Context => T)(implicit ctx: Context): String = {
    val nestedCtx = ctx.fresh.withTypeComparerFn(new ExplainingTypeComparer(_))
    op(nestedCtx)
    nestedCtx.typeComparer.toString
  }
}

class ExplainingTypeComparer(initctx: Context) extends TypeComparer(initctx) {
  private var indent = 0
  private val b = new StringBuilder

  private var skipped = false

  override def traceIndented[T](str: String)(op: => T): T =
    if (skipped)
      op
/*
    else if (str startsWith " =+ scala.collection.immutable.List <:<  =+ scala.collection.immutable.List") {
      skipped = true
      try op
      finally skipped = false
    }*/ else {
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
    traceIndented(s"${show(tp1)} <:< ${show(tp2)}") {
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

  override  def addConstraint(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean =
    traceIndented(s"add constraint $param ${if (fromBelow) ">:" else "<:"} $bound $frozenConstraint") {
      super.addConstraint(param, bound, fromBelow)
    }

  override def copyIn(ctx: Context) = new ExplainingTypeComparer(ctx)

  override def toString =
    "Subtype trace:" + {
      try b.toString
      finally b.clear()
  }
}