package dotty.tools.dotc.core

import Types._, Symbols._, Contexts._

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
trait Substituters { this: Context =>

  final def subst(tp: Type, from: BindingType, to: BindingType, map: SubstBindingMap): Type =
    tp match {
      case tp: BoundType =>
        if (tp.binder eq from) tp.copy(to.asInstanceOf[tp.BT]) else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(subst(tp.prefix, from, to, map))
      case _: ThisType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst(tp.parent, from, to, map), tp.refinedName, subst(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstBindingMap(from, to))
          .mapOver(tp)
    }

  final def subst1(tp: Type, from: Symbol, to: Type, map: Subst1Map): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym eq from) return to
        if (sym.isStatic && !from.isStatic) tp
        else tp.derivedNamedType(subst1(tp.prefix, from, to, map))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst1(tp.parent, from, to, map), tp.refinedName, subst1(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new Subst1Map(from, to))
          .mapOver(tp)
    }
  }

  final def subst2(tp: Type, from1: Symbol, to1: Type, from2: Symbol, to2: Type, map: Subst2Map): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym eq from1) return to1
        if (sym eq from2) return to2
        if (sym.isStatic && !from1.isStatic && !from2.isStatic) tp
        else tp.derivedNamedType(subst2(tp.prefix, from1, to1, from2, to2, map))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst2(tp.parent, from1, to1, from2, to2, map), tp.refinedName, subst2(tp.refinedInfo, from1, to1, from2, to2, map))
      case _ =>
        (if (map != null) map else new Subst2Map(from1, to1, from2, to2))
          .mapOver(tp)
    }
  }

  final def subst(tp: Type, from: List[Symbol], to: List[Type], map: SubstMap): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        var fs = from
        var ts = to
        while (fs.nonEmpty) {
          if (fs.head eq sym) return ts.head
          fs = fs.tail
          ts = ts.tail
        }
        if (sym.isStatic && !existsStatic(from)) tp
        else tp.derivedNamedType(subst(tp.prefix, from, to, map))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst(tp.parent, from, to, map), tp.refinedName, subst(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstMap(from, to))
          .mapOver(tp)
    }
  }

  final def substSym(tp: Type, from: List[Symbol], to: List[Symbol], map: SubstSymMap): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        var fs = from
        var ts = to
        while (fs.nonEmpty) {
          if (fs.head eq sym) return NamedType.withSym(tp.prefix, ts.head)
          fs = fs.tail
          ts = ts.tail
        }
        if (sym.isStatic && !existsStatic(from)) tp
        else tp.derivedNamedType(substSym(tp.prefix, from, to, map))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substSym(tp.parent, from, to, map), tp.refinedName, substSym(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstSymMap(from, to))
          .mapOver(tp)
    }

  final def substThis(tp: Type, from: ClassSymbol, to: Type, map: SubstThisMap): Type =
    tp match {
      case tp @ ThisType(clazz) =>
        if (clazz eq from) to else tp
      case tp: NamedType =>
        if (tp.symbol.isStaticOwner) tp
        else tp.derivedNamedType(substThis(tp.prefix, from, to, map))
      case _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substThis(tp.parent, from, to, map), tp.refinedName, substThis(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstThisMap(from, to))
          .mapOver(tp)
    }

  final def substThis(tp: Type, from: RefinedType, to: Type, map: SubstRefinedThisMap): Type =
    tp match {
      case tp @ RefinedThis(rt) =>
        if (rt eq from) to else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(substThis(tp.prefix, from, to, map))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substThis(tp.parent, from, to, map), tp.refinedName, substThis(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstRefinedThisMap(from, to))
          .mapOver(tp)
    }

  final def substParam(tp: Type, from: ParamType, to: Type, map: SubstParamMap): Type =
    tp match {
      case tp: BoundType =>
        if (tp == from) to else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(substParam(tp.prefix, from, to, map))
      case _: ThisType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substParam(tp.parent, from, to, map), tp.refinedName, substParam(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstParamMap(from, to))
          .mapOver(tp)
    }

  final def substParams(tp: Type, from: BindingType, to: List[Type], map: SubstParamsMap): Type =
    tp match {
      case tp: ParamType =>
        if (tp.binder == from) to(tp.paramNum) else tp
      case tp: NamedType =>
        if (tp.symbol.isStatic) tp
        else tp.derivedNamedType(substParams(tp.prefix, from, to, map))
      case _: ThisType | NoPrefix | _: RefinedThis =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substParams(tp.parent, from, to, map), tp.refinedName, substParams(tp.refinedInfo, from, to, map))
      case _ =>
        (if (map != null) map else new SubstParamsMap(from, to))
          .mapOver(tp)
    }

  private def existsStatic(syms: List[Symbol]): Boolean = syms match {
    case sym :: syms1 => sym.isStatic || existsStatic(syms1)
    case nil => false
  }

  final class SubstBindingMap(from: BindingType, to: BindingType) extends TypeMap {
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

  final class SubstSymMap(from: List[Symbol], to: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = substSym(tp, from, to, this)
  }

  final class SubstThisMap(from: ClassSymbol, to: Type) extends TypeMap {
    def apply(tp: Type): Type = substThis(tp, from, to, this)
  }

  final class SubstRefinedThisMap(from: RefinedType, to: Type) extends TypeMap {
    def apply(tp: Type): Type = substThis(tp, from, to, this)
  }

  final class SubstParamMap(from: ParamType, to: Type) extends TypeMap {
    def apply(tp: Type) = substParam(tp, from, to, this)
  }

  final class SubstParamsMap(from: BindingType, to: List[Type]) extends TypeMap {
    def apply(tp: Type) = substParams(tp, from, to, this)
  }
}