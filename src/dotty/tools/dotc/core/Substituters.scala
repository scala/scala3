package dotty.tools.dotc.core

import Types._, Symbols._, Contexts._, Names._

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
trait Substituters { this: Context =>

  final def subst(tp: Type, from: BindingType, to: BindingType, theMap: SubstBindingMap): Type =
    tp match {
      case tp: BoundType =>
        if (tp.binder eq from) tp.copyBoundType(to.asInstanceOf[tp.BT]) else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStatic) tp
        else tp.derivedSelect(subst(tp.prefix, from, to, theMap))
      case _: ThisType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst(tp.parent, from, to, theMap), tp.refinedName, subst(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(subst(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstBindingMap(from, to))
          .mapOver(tp)
    }

  final def subst1(tp: Type, from: Symbol, to: Type, theMap: Subst1Map): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym eq from) return to
        if (sym.isStatic && !from.isStatic) tp
        else tp.derivedSelect(subst1(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst1(tp.parent, from, to, theMap), tp.refinedName, subst1(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(subst1(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new Subst1Map(from, to))
          .mapOver(tp)
    }
  }

  final def subst2(tp: Type, from1: Symbol, to1: Type, from2: Symbol, to2: Type, theMap: Subst2Map): Type = {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym eq from1) return to1
        if (sym eq from2) return to2
        if (sym.isStatic && !from1.isStatic && !from2.isStatic) tp
        else tp.derivedSelect(subst2(tp.prefix, from1, to1, from2, to2, theMap))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst2(tp.parent, from1, to1, from2, to2, theMap), tp.refinedName, subst2(tp.refinedInfo, from1, to1, from2, to2, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(subst2(tp.alias, from1, to1, from2, to2, theMap))
      case _ =>
        (if (theMap != null) theMap else new Subst2Map(from1, to1, from2, to2))
          .mapOver(tp)
    }
  }

  final def subst(tp: Type, from: List[Symbol], to: List[Type], theMap: SubstMap): Type = {
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
        else tp.derivedSelect(subst(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(subst(tp.parent, from, to, theMap), tp.refinedName, subst(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(subst(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstMap(from, to))
          .mapOver(tp)
    }
  }

  final def substDealias(tp: Type, from: List[Symbol], to: List[Type], theMap: SubstDealiasMap): Type = {
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
        else {
          val prefix1 = substDealias(tp.prefix, from, to, theMap)
          if (prefix1 ne tp.prefix) tp.derivedSelect(prefix1)
          else if (sym.isAliasType) {
            val hi = sym.info.bounds.hi
            val hi1 = substDealias(hi, from, to, theMap)
            if (hi1 eq hi) tp else hi1
          }
          else tp
        }
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substDealias(tp.parent, from, to, theMap), tp.refinedName, substDealias(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substDealias(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstDealiasMap(from, to))
          .mapOver(tp)
    }
  }

  final def substSym(tp: Type, from: List[Symbol], to: List[Symbol], theMap: SubstSymMap): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        var fs = from
        var ts = to
        while (fs.nonEmpty) {
          if (fs.head eq sym)
            return tp match {
              case tp: WithFixedSym => NamedType.withFixedSym(tp.prefix, ts.head)
              case _ => substSym(tp.prefix, from, to, theMap) select ts.head
            }
          fs = fs.tail
          ts = ts.tail
        }
        if (sym.isStatic && !existsStatic(from)) tp
        else tp.derivedSelect(substSym(tp.prefix, from, to, theMap))
      case tp: ThisType =>
        val sym = tp.cls
        var fs = from
        var ts = to
        while (fs.nonEmpty) {
          if (fs.head eq sym) return ts.head.asClass.thisType
          fs = fs.tail
          ts = ts.tail
        }
        tp
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substSym(tp.parent, from, to, theMap), tp.refinedName, substSym(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substSym(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstSymMap(from, to))
          .mapOver(tp)
    }

  final def substThis(tp: Type, from: ClassSymbol, to: Type, theMap: SubstThisMap): Type =
    tp match {
      case tp: ThisType =>
        if (tp.cls eq from) to else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStaticOwner) tp
        else tp.derivedSelect(substThis(tp.prefix, from, to, theMap))
      case _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substThis(tp.parent, from, to, theMap), tp.refinedName, substThis(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substThis(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstThisMap(from, to))
          .mapOver(tp)
    }

  final def substRefinedThis(tp: Type, from: Type, to: Type, theMap: SubstRefinedThisMap): Type =
    tp match {
      case tp @ RefinedThis(binder) =>
        if (binder eq from) to else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStatic) tp
        else tp.derivedSelect(substRefinedThis(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substRefinedThis(tp.parent, from, to, theMap), tp.refinedName, substRefinedThis(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substRefinedThis(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstRefinedThisMap(from, to))
          .mapOver(tp)
    }

  final def substRecThis(tp: Type, from: Type, to: Type, theMap: SubstRecThisMap): Type =
    tp match {
      case tp @ RecThis(binder) =>
        if (binder eq from) to else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStatic) tp
        else tp.derivedSelect(substRecThis(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substRecThis(tp.parent, from, to, theMap), tp.refinedName, substRecThis(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substRecThis(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstRecThisMap(from, to))
          .mapOver(tp)
    }

  final def substParam(tp: Type, from: ParamType, to: Type, theMap: SubstParamMap): Type =
    tp match {
      case tp: BoundType =>
        if (tp == from) to else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStatic) tp
        else tp.derivedSelect(substParam(tp.prefix, from, to, theMap))
      case _: ThisType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substParam(tp.parent, from, to, theMap), tp.refinedName, substParam(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substParam(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstParamMap(from, to))
          .mapOver(tp)
    }

  final def substParams(tp: Type, from: BindingType, to: List[Type], theMap: SubstParamsMap): Type =
    tp match {
      case tp: ParamType =>
        if (tp.binder == from) to(tp.paramNum) else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStatic) tp
        else tp.derivedSelect(substParams(tp.prefix, from, to, theMap))
      case _: ThisType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(substParams(tp.parent, from, to, theMap), tp.refinedName, substParams(tp.refinedInfo, from, to, theMap))
      case tp: TypeAlias =>
        tp.derivedTypeAlias(substParams(tp.alias, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstParamsMap(from, to))
          .mapOver(tp)
    }

  private def existsStatic(syms: List[Symbol]): Boolean = syms match {
    case sym :: syms1 => sym.isStatic || existsStatic(syms1)
    case nil => false
  }

  final class SubstBindingMap(from: BindingType, to: BindingType) extends DeepTypeMap {
    def apply(tp: Type) = subst(tp, from, to, this)
  }

  final class Subst1Map(from: Symbol, to: Type) extends DeepTypeMap {
    def apply(tp: Type) = subst1(tp, from, to, this)
  }

  final class Subst2Map(from1: Symbol, to1: Type, from2: Symbol, to2: Type) extends DeepTypeMap {
    def apply(tp: Type) = subst2(tp, from1, to1, from2, to2, this)
  }

  final class SubstMap(from: List[Symbol], to: List[Type]) extends DeepTypeMap {
    def apply(tp: Type): Type = subst(tp, from, to, this)
  }

  final class SubstDealiasMap(from: List[Symbol], to: List[Type]) extends DeepTypeMap {
    override def apply(tp: Type): Type = substDealias(tp, from, to, this)
  }

  final class SubstSymMap(from: List[Symbol], to: List[Symbol]) extends DeepTypeMap {
    def apply(tp: Type): Type = substSym(tp, from, to, this)
  }

  final class SubstThisMap(from: ClassSymbol, to: Type) extends DeepTypeMap {
    def apply(tp: Type): Type = substThis(tp, from, to, this)
  }

  final class SubstRefinedThisMap(from: Type, to: Type) extends DeepTypeMap {
    def apply(tp: Type): Type = substRefinedThis(tp, from, to, this)
  }

  final class SubstRecThisMap(from: Type, to: Type) extends DeepTypeMap {
    def apply(tp: Type): Type = substRecThis(tp, from, to, this)
  }

  final class SubstParamMap(from: ParamType, to: Type) extends DeepTypeMap {
    def apply(tp: Type) = substParam(tp, from, to, this)
  }

  final class SubstParamsMap(from: BindingType, to: List[Type]) extends DeepTypeMap {
    def apply(tp: Type) = substParams(tp, from, to, this)
  }

  /** A map for "cycle safe substitutions" which do not force the denotation
   *  of a TypeRef unless the name matches up with one of the substituted symbols.
   */
  final class SafeSubstMap(from: List[Symbol], to: List[Type]) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType =>
        try {
          var sym: Symbol = null
          var fs = from
          var ts = to
          while (fs.nonEmpty) {
            if (fs.head.name == tp.name) {
              if (sym == null) sym = tp.symbol
              if (fs.head eq sym) return ts.head
            }
            fs = fs.tail
            ts = ts.tail
          }
          tp.newLikeThis(apply(tp.prefix))
        }
        catch {
          case ex: CyclicReference => tp.derivedSelect(apply(tp.prefix))
        }
      case _ => mapOver(tp)
    }
  }
}
