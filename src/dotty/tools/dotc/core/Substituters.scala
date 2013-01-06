package dotty.tools.dotc.core

import Types._, Symbols._, Contexts._

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
object Substituters {

  class SubstOps(val tp: Type) extends AnyVal {

    def subst(from: PolyType, to: PolyType, map: SubstPolyMap = null)(implicit ctx: Context): Type =
      tp match {
        case tp @ PolyParam(pt, n) =>
          if (pt eq from) PolyParam(to, n) else tp
        case tp: NamedType =>
          if (tp.symbol.isStatic) tp
          else tp.derivedNamedType(tp.prefix.subst(from, to, map), tp.name)
        case ThisType(_)
           | MethodParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new SubstPolyMap(from, to)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }

    def subst(from: MethodType, to: MethodType, map: SubstMethodMap)(implicit ctx: Context): Type =
      tp match {
        case tp @ MethodParam(mt, n) =>
          if (mt eq from) MethodParam(to, n) else tp
        case tp: NamedType =>
          if (tp.symbol.isStatic) tp
          else tp.derivedNamedType(tp.prefix.subst(from, to, map), tp.name)
        case ThisType(_)
           | PolyParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new SubstMethodMap(from, to)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }

    def subst1(from: Symbol, to: Type, map: Subst1Map)(implicit ctx: Context): Type = {
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (tp.prefix eq NoPrefix) {
            if (sym eq from) return to
          }
          if (sym.isStatic) tp
          else tp.derivedNamedType(tp.prefix.subst1(from, to, map), tp.name)
        case ThisType(_)
           | MethodParam(_, _)
           | PolyParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new Subst1Map(from, to)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }
    }

    def subst2(from1: Symbol, to1: Type, from2: Symbol, to2: Type, map: Subst2Map)(implicit ctx: Context): Type = {
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (tp.prefix eq NoPrefix) {
            if (sym eq from1) return to1
            if (sym eq from2) return to2
          }
          if (sym.isStatic) tp
          else tp.derivedNamedType(tp.prefix.subst2(from1, to1, from2, to2, map), tp.name)
        case ThisType(_)
           | MethodParam(_, _)
           | PolyParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new Subst2Map(from1, to1, from2, to2)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }
    }

    def subst(from: List[Symbol], to: List[Type], map: SubstMap)(implicit ctx: Context): Type = {
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (tp.prefix eq NoPrefix) {
            var fs = from
            var ts = to
            while (fs.nonEmpty) {
              if (fs.head eq sym) return ts.head
              fs = fs.tail
              ts = ts.tail
            }
          }
          if (sym.isStatic) tp
          else tp.derivedNamedType(tp.prefix.subst(from, to, map), tp.name)
        case ThisType(_)
           | MethodParam(_, _)
           | PolyParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new SubstMap(from, to)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }
    }

    def substThis(from: ClassSymbol, to: Type, map: SubstThisMap)(implicit ctx: Context): Type =
      tp match {
        case tp @ ThisType(clazz) =>
          if (clazz eq from) to else tp
        case tp: NamedType =>
          if (tp.symbol.isStatic) tp
          else tp.derivedNamedType(tp.prefix.substThis(from, to, map), tp.name)
        case MethodParam(_, _)
           | PolyParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new SubstThisMap(from, to)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }

    def substThis(from: RefinedType, to: Type, map: SubstRefinedThisMap)(implicit ctx: Context): Type =
      tp match {
        case tp @ RefinedThis(rt) =>
          if (rt eq from) to else tp
        case tp: NamedType =>
          if (tp.symbol.isStatic) tp
          else tp.derivedNamedType(tp.prefix.substThis(from, to, map), tp.name)
        case ThisType(_)
           | MethodParam(_, _)
           | PolyParam(_, _)
           | NoPrefix => tp
        case _ =>
          val substMap = if (map != null) map else new SubstRefinedThisMap(from, to)
          tp match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                substMap(tp.tycon), tp.typeArgs mapConserve substMap)
            case _ =>
              substMap mapOver tp
          }
      }
  }

  class SubstPolyMap(from: PolyType, to: PolyType)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp.subst(from, to, this)
  }

  class SubstMethodMap(from: MethodType, to: MethodType)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp.subst(from, to, this)
  }

  class Subst1Map(from: Symbol, to: Type)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp.subst1(from, to, this)
  }

  class Subst2Map(from1: Symbol, to1: Type, from2: Symbol, to2: Type)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp.subst2(from1, to1, from2, to2, this)
  }

  class SubstMap(from: List[Symbol], to: List[Type])(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type): Type = tp.subst(from, to, this)
  }

  class SubstThisMap(from: ClassSymbol, to: Type)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type): Type = tp.substThis(from, to, this)
  }

  class SubstRefinedThisMap(from: RefinedType, to: Type)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type): Type = tp.substThis(from, to, this)
  }
}