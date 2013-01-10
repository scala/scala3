package dotty.tools.dotc.core

import Types._, Symbols._, Contexts._

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
trait Substituters { this: Context =>

  final def subst(tp: Type, from: PolyType, to: PolyType, map: SubstPolyMap = null): Type =
    tp match {
      case tp @ PolyParam(pt, n) =>
        if (pt eq from) PolyParam(to, n) else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(subst(tp.prefix, from, to, map), tp.name)
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

  final def subst(tp: Type, from: MethodType, to: MethodType, map: SubstMethodMap): Type =
    tp match {
      case tp @ MethodParam(mt, n) =>
        if (mt eq from) MethodParam(to, n) else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(subst(tp.prefix, from, to, map), tp.name)
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

  final def subst1(tp: Type, from: Symbol, to: Type, map: Subst1Map): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (tp.prefix eq NoPrefix) {
          if (sym eq from) return to
        }
        if (sym.isStatic) tp
        else tp.derivedNamedType(subst1(tp.prefix, from, to, map), tp.name)
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

  final def subst2(tp: Type, from1: Symbol, to1: Type, from2: Symbol, to2: Type, map: Subst2Map): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (tp.prefix eq NoPrefix) {
          if (sym eq from1) return to1
          if (sym eq from2) return to2
        }
        if (sym.isStatic) tp
        else tp.derivedNamedType(subst2(tp.prefix, from1, to1, from2, to2, map), tp.name)
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

  final def subst(tp: Type, from: List[Symbol], to: List[Type], map: SubstMap): Type = {
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
        else tp.derivedNamedType(subst(tp.prefix, from, to, map), tp.name)
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

  final def substThis(tp: Type, from: ClassSymbol, to: Type, map: SubstThisMap): Type =
    tp match {
      case tp @ ThisType(clazz) =>
        if (clazz eq from) to else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(substThis(tp.prefix, from, to, map), tp.name)
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

  final def substThis(tp: Type, from: RefinedType, to: Type, map: SubstRefinedThisMap): Type =
    tp match {
      case tp @ RefinedThis(rt) =>
        if (rt eq from) to else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(substThis(tp.prefix, from, to, map), tp.name)
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

  final class SubstPolyMap(from: PolyType, to: PolyType) extends TypeMap {
    def apply(tp: Type) = subst(tp, from, to, this)
  }

  final class SubstMethodMap(from: MethodType, to: MethodType) extends TypeMap {
    def apply(tp: Type) = subst(tp, from, to, this)
  }

  final class Subst1Map(from: Symbol, to: Type) extends TypeMap {
    def apply(tp: Type) = subst1(tp, from, to, this)
  }

  final class Subst2Map(from1: Symbol, to1: Type, from2: Symbol, to2: Type) extends TypeMap {
    def apply(tp: Type) = subst2(tp, from1, to1, from2, to2, this)
  }

  final class SubstMap(from: List[Symbol], to: List[Type]) extends TypeMap {
    def apply(tp: Type): Type = subst(tp, from, to, this)
  }

  final class SubstThisMap(from: ClassSymbol, to: Type) extends TypeMap {
    def apply(tp: Type): Type = substThis(tp, from, to, this)
  }

  final class SubstRefinedThisMap(from: RefinedType, to: Type) extends TypeMap {
    def apply(tp: Type): Type = substThis(tp, from, to, this)
  }
}