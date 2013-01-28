package dotty.tools.dotc.core

import Types._, Contexts._, Symbols._, Flags._
import collection.mutable

object TypeComparers {

  type Constraints = Map[PolyParam, TypeBounds]

  object TypeComparer {
    private final val LogPendingSubTypesThreshold = 50
  }

  class TypeComparer(_ctx: Context) extends DotClass {
    import TypeComparer._

    implicit val ctx = _ctx

    var constraints = ctx.constraints

    private var pendingSubTypes: mutable.Set[(Type, Type)] = null
    private var recCount = 0

    def addConstraint(param: PolyParam, bounds: TypeBounds): Boolean = {
      val newbounds = constraints(param) & bounds
      constraints = constraints.updated(param, newbounds)
      newbounds.lo <:< newbounds.hi
    }

    def isSubType(tp1: Type, tp2: Type): Boolean =
      if (tp1 == NoType || tp2 == NoType) false
      else if (tp1 eq tp2) true
      else {
        val cs = constraints
        try {
          recCount += 1
          val result =
            if (recCount < LogPendingSubTypesThreshold) firstTry(tp1, tp2)
            else monitoredIsSubType(tp1, tp2)
          recCount -= 1
          if (!result) constraints = cs
          result
        } catch {
          case ex: Throwable =>
            recCount -= 1
            constraints = cs
            throw ex
        }
      }

    def monitoredIsSubType(tp1: Type, tp2: Type) = {
      if (pendingSubTypes == null)
        pendingSubTypes = new mutable.HashSet[(Type, Type)]
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

    def firstTry(tp1: Type, tp2: Type): Boolean = tp2 match {
      case tp2: TypeRef =>
        tp1 match {
          case tp1: TypeRef =>
            val sym1 = tp1.symbol
            val sym2 = tp2.symbol
            val pre1 = tp1.prefix
            val pre2 = tp2.prefix
            (sym1 == sym2 && (
              ctx.erasedTypes ||
              (sym1.owner.isPackage) ||
              isSubType(pre1, pre2))
              ||
              tp1.name == tp2.name &&
              isSubType(pre1, pre2) &&
              (sym2.info.isRealTypeBounds || isSubType(pre2, pre1)) // ???
              ||
              (sym2.isClass) && {
                val base = tp1.baseType(sym2)
                (base ne tp1) && isSubType(base, tp2)
              }
              ||
              thirdTryRef(tp1, tp2))
          case _ =>
            secondTry(tp1, tp2)
        }
      case WildcardType | ErrorType =>
        true
      case tp2: PolyParam if (constraints contains tp2) =>
        addConstraint(tp2, TypeBounds(tp1, defn.AnyType))
      case _ =>
        secondTry(tp1, tp2)
    }


    def secondTry(tp1: Type, tp2: Type): Boolean = tp1 match {
      case tp1: PolyParam if (constraints contains tp1) =>
        addConstraint(tp1, TypeBounds(defn.NothingType, tp2))
      case WildcardType | ErrorType =>
        true
      case _ =>
        thirdTry(tp1, tp2)
    }

    def thirdTryRef(tp1: Type, tp2: TypeRef): Boolean = (
      (tp2 == defn.SingletonType && tp1.isStable)
      ||
      (!tp2.symbol.isClass && isSubType(tp1, tp2.info.bounds.lo))
      ||
      fourthTry(tp1, tp2)
    )

    def thirdTry(tp1: Type, tp2: Type): Boolean = tp2 match {
      case tp2: TypeRef =>
        thirdTryRef(tp1, tp2)
      case tp2: RefinedType =>
        isSubType(tp1, tp2.parent) &&
        isSubType(tp1.member(tp2.name).info, tp2.info)
      case AndType(tp21, tp22) =>
        isSubType(tp1, tp21) && isSubType(tp1, tp22)
      case OrType(tp21, tp22) =>
        isSubType(tp1, tp21) || isSubType(tp1, tp22)
      case tp2 @ MethodType(_, formals1) =>
        tp1 match {
          case tp1 @ MethodType(_, formals2) =>
            tp1.signature == tp2.signature &&
            matchingParams(formals1, formals2, tp1.isJava, tp2.isJava) &&
            tp1.isImplicit == tp2.isImplicit &&
            isSubType(tp1.resultType, tp2.resultType.subst(tp2, tp1))
          case _ =>
            false
        }
      case tp2: PolyType =>
        tp1 match {
          case tp1: PolyType =>
            tp1.signature == tp2.signature &&
            (tp1.paramBounds corresponds tp2.paramBounds)(
              (b1, b2) => b1 =:= b2.subst(tp2, tp1)) &&
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
            isSubType(lo2, lo1) && isSubType(hi1, hi2)
          case tp1: ClassInfo =>
            val tt = tp1.typeConstructor // was typeTemplate
            lo2 <:< tt && tt <:< hi2
          case _ =>
            false
        }
      case _ =>
        fourthTry(tp1, tp2)
    }

    def fourthTry(tp1: Type, tp2: Type): Boolean =  tp1 match {
      case tp1: TypeRef =>
        ((tp1 eq defn.NothingType)
         ||
         (tp1 eq defn.NullType) && tp2.typeSymbol.isNonValueClass
         ||
         (!tp1.symbol.isClass && isSubType(tp1.info.bounds.hi, tp2)))
      case tp1: RefinedType =>
        isSubType(tp1.parent, tp2)
      case AndType(tp11, tp12) =>
        isSubType(tp11, tp2) || isSubType(tp12, tp2)
      case OrType(tp11, tp12) =>
        isSubType(tp11, tp2) && isSubType(tp12, tp2)
      case tp1: SingletonType =>
        isSubType(tp1.underlying, tp2)
      case _ =>
        false
    }

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
            matchesType(tp1.resultType, tp2, alwaysMatchSimple)
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
          case _ =>
            alwaysMatchSimple || isSameType(tp1, tp2)
        }
      }

    /** Are `syms1` and `syms2` parameter lists with pairwise equivalent types? */
    private def matchingParams(formals1: List[Type], formals2: List[Type], isJava1: Boolean, isJava2: Boolean): Boolean = formals1 match {
      case Nil =>
        formals2.isEmpty
      case formal1 :: rest1 =>
        formals2 match {
          case Nil =>
            false
          case formal2 :: rest2 =>
            (formal1 =:= formal2 ||
              isJava1 && formal2 == defn.ObjectType && formal1 == defn.AnyType ||
              isJava2 && formal1 == defn.ObjectType && formal2 == defn.AnyType) &&
              matchingParams(rest1, rest2, isJava1, isJava2)
        }
    }

    def isSameType(tp1: Type, tp2: Type): Boolean =
      if (tp1 == NoType || tp2 == NoType) false
      else if (tp1 eq tp2) true
      else isSubType(tp1, tp2) && isSubType(tp2, tp1)
  }
}