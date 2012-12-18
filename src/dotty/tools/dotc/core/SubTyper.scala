package dotty.tools.dotc.core

import Types._, Contexts._, Symbols._

object SubTypers {

  type Constraints = Map[PolyParam, TypeBounds]

  class SubTyper extends DotClass {

    var constraints: Constraints = _
    implicit var ctx: Context = _

    def init(constraints: Constraints)(implicit ctx: Context): SubTyper = {
      this.constraints = constraints
      this.ctx = ctx
      this
    }

    def addConstraint(param: PolyParam, bounds: TypeBounds): Boolean = {
      val newbounds = constraints(param) & bounds
      constraints = constraints.updated(param, newbounds)
      newbounds.lo <:< newbounds.hi
    }

    def isSubType(tp1: Type, tp2: Type): Boolean = {
      if (tp1 == NoType || tp2 == NoType) return false
      if (tp1 eq tp2) return true
      val cs = constraints
      try {
        val result = firstTry(tp1, tp2)
        if (!result) constraints = cs
        result
      } catch {
        case ex: Throwable =>
          constraints = cs
          throw ex
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
                sym1.owner.hasFlag(Flags.Package) ||
                isSubType(pre1, pre2))
               ||
               tp1.name == tp2.name &&
               isSubType(pre1, pre2) &&
               (sym2.isAbstractType || isSubType(pre2, pre1))
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
        case tp2: PolyParam if (constraints contains tp2) =>
          addConstraint(tp2, TypeBounds(tp1, AnyType))
        case _ =>
          secondTry(tp1, tp2)
      }


    def secondTry(tp1: Type, tp2: Type): Boolean = tp1 match {
      case tp1: PolyParam if (constraints contains tp1) =>
        addConstraint(tp1, TypeBounds(NothingType, tp2))
      case _ =>
        thirdTry(tp1, tp2)
    }

    def thirdTryRef(tp1: Type, tp2: TypeRef): Boolean = (
      (tp2 == SingletonType && tp1.isStable)
      ||
      (!tp2.symbol.isClass && isSubType(tp1, tp2.info.bounds.lo))
      ||
      fourthTry(tp1, tp2)
    )

    def thirdTry(tp1: Type, tp2: Type): Boolean = ???
    def fourthTry(tp1: Type, tp2: Type): Boolean = ???
  }
}