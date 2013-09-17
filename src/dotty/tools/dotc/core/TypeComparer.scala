package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Flags._
import StdNames.nme
import collection.mutable
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

  private var frozenConstraint = false

  private var myAnyType: Type = null
  private var myNothingType: Type = null
  private var myNullType: Type = null
  private var myObjectType: Type = null
  def AnyType = {
    if (myAnyType == null) myAnyType = defn.AnyType
    myAnyType
  }
  def NothingType = {
    if (myNothingType == null) myNothingType = defn.NothingType
    myNothingType
  }
  def NullType = {
    if (myNullType == null) myNullType = defn.NullType
    myNullType
  }
  def ObjectType = {
    if (myObjectType == null) myObjectType = defn.ObjectType
    myObjectType
  }

  /** Add the constraint `<bounds.lo <: param <: bounds.hi>`
   *  to `constraint`.
   *  @pre `param` is in the constraint's domain
   */
  def addConstraint(param: PolyParam, bounds: TypeBounds): Boolean =
    !frozenConstraint && {
      val pt = param.binder
      val pnum = param.paramNum
      val oldEntries = constraint(pt)
      val oldBounds = oldEntries(pnum).asInstanceOf[TypeBounds]
      val newBounds = oldBounds & bounds
      if (oldBounds ne newBounds) {
        val newEntries = oldEntries.clone
        newEntries(pnum) = newBounds
        constraint = constraint.updated(pt, newEntries)
      }
      isSubType(newBounds.lo, newBounds.hi)
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
      case tp2: PolyParam =>
        //println(constraint.show)
        constraint(tp2) match {
          case TypeBounds(lo, _) => isSubType(tp1, lo) || addConstraint(tp2, TypeBounds.lower(tp1))
          case _ => secondTry(tp1, tp2)
        }
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
    case tp1: PolyParam =>
      constraint(tp1) match {
        case TypeBounds(_, hi) => isSubType(hi, tp2) || addConstraint(tp1, TypeBounds.upper(tp2))
        case _ => thirdTry(tp1, tp2)
      }
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
    case tp2: RefinedType =>
      isSubType(tp1, tp2.parent) && (
        tp2.refinedName == nme.WILDCARD
        || (tp1 eq NothingType)
        || (tp1 eq NullType)
        || tp1.member(tp2.refinedName).hasAltWith(alt =>
             isSubType(alt.info, tp2.refinedInfo)))
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
            (tp1.paramBounds corresponds tp2.paramBounds)((b1, b2) =>
              isSameType(b1, b2.subst(tp2, tp1))) &&
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
        case _ =>
          false
      }
    case tp2 @ ExprType(restpe1) =>
      tp1 match {
        case tp1 @ ExprType(restpe2) =>
          isSubType(restpe1, restpe2)
        case _ =>
          false
      }
    case TypeBounds(lo2, hi2) =>
      tp1 match {
        case TypeBounds(lo1, hi1) =>
          ((lo2 eq NothingType) || isSubType(lo2, lo1)) &&
          ((hi2 eq AnyType) || isSubType(hi1, hi2))
        case tp1: ClassInfo =>
          val tt = tp1.typeConstructor // was typeTemplate
          isSubType(lo2, tt) && isSubType(tt, hi2)
        case _ =>
          false
      }
    /* needed?
       case ClassInfo(pre2, denot2) =>
        tp1 match {
          case ClassInfo(pre1, denot1) =>
            (denot1 eq denot2) && isSubType(pre2, pre1) // !!! or isSameType?
        }
*/
    case _ =>
      fourthTry(tp1, tp2)
  }

  def fourthTry(tp1: Type, tp2: Type): Boolean = tp1 match {
    case tp1: TypeRef =>
      ((tp1 eq NothingType)
        || (tp1 eq NullType) && tp2.dealias.typeSymbol.isNonValueClass
        || (tp1.info match {
              case TypeBounds(lo1, hi1) =>
                isSubType(hi1, tp2) ||
                (tp1.symbol is GADTFlexType) && trySetType(tp1, TypeBounds(lo1, hi1 & tp2))
              case _ => false
           }))
    case tp1: SingletonType =>
      isSubType(tp1.underlying, tp2)
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
            || isJava1 && formal2 == ObjectType && formal1 == AnyType
            || isJava2 && formal1 == ObjectType && formal2 == AnyType) && matchingParams(rest1, rest2, isJava1, isJava2)
        case nil =>
          false
      }
    case nil =>
      formals2.isEmpty
  }

  def isSameType(tp1: Type, tp2: Type): Boolean =
    if (tp1 == NoType || tp2 == NoType) false
    else if (tp1 eq tp2) true
    else isSubType(tp1, tp2) && isSubType(tp2, tp1)

  def glb(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else if (!tp1.exists || (tp1 eq AnyType) || (tp2 eq NothingType)) tp2
    else if (!tp2.exists || (tp2 eq AnyType) || (tp1 eq NothingType)) tp1
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
              else {
                tp1 match {
                  case tp1: TypeBounds =>
                    tp2 match {
                      case tp2: TypeBounds =>
                        return TypeBounds(tp1.lo | tp2.lo, tp1.hi & tp2.hi)
                      case tp2: ClassInfo =>
                        throw new ClassMergeError(tp2, tp1)
                      case _ =>
                    }
                  case tp1: ClassInfo =>
                    throw new ClassMergeError(tp1, tp2)
                  case _ =>
                }
                AndType(tp1, tp2)
              }
            }
        }
    }

  final def glb(tps: List[Type]): Type =
    (AnyType /: tps)(glb)

  def lub(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else if (!tp1.exists || (tp1 eq AnyType) || (tp2 eq NothingType)) tp1
    else if (!tp2.exists || (tp2 eq AnyType) || (tp1 eq NothingType)) tp2
    else {
      val t1 = mergeIfSuper(tp1, tp2)
      if (t1.exists) t1
      else {
        val t2 = mergeIfSuper(tp2, tp1)
        if (t2.exists) t2
        else {
          tp1 match {
            case tp1: TypeBounds =>
              tp2 match {
                case tp2: TypeBounds =>
                  return TypeBounds(tp1.lo & tp2.lo, tp1.hi | tp2.hi)
                case tp2: ClassInfo =>
                  throw new ClassMergeError(tp2, tp1)
                case _ =>
              }
            case tp1: ClassInfo =>
              throw new ClassMergeError(tp1, tp2)
            case _ =>
          }
          OrType(tp1, tp2)
        }
      }
    }

  final def lub(tps: List[Type]): Type =
    (NothingType /: tps)(lub)

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

  def copyIn(ctx: Context) = new TypeComparer(ctx)
}

class ExplainingTypeComparer(initctx: Context) extends TypeComparer(initctx) {
  private var indent = 0
  private val b = new StringBuilder

  def traceIndented[T](str: String)(op: => T): T = {
    assert(str != "<notype> <:< Int")
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
    traceIndented(s"${show(tp1)} <:< ${show(tp2)} ${tp1.getClass} ${defn.NothingType.getClass} ${tp1.normalizedPrefix} ${defn.NothingType.normalizedPrefix} ${tp1 eq defn.NothingType}  ${tp1.typeSymbol eq defn.NothingClass}") {
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

  override def copyIn(ctx: Context) = new ExplainingTypeComparer(ctx)

  override def toString =
    "Subtype trace:" + {
      try b.toString
      finally b.clear()
  }
}