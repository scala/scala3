package dotty.tools.dotc.core

import Types._, Symbols._, Contexts._, Names._

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
trait Substituters { this: Context =>

  final def subst(tp: Type, from: BindingType, to: BindingType): Type =
    tp match {
      case tp: NamedType => substNamed(tp, from, to)
      case tp: BoundType => substBound(tp, from, to)
      case _: ThisType => tp
      case _ => new SubstBindingMap(from, to).mapOver2(tp)
    }

  private def substNamed(tp: NamedType, from: BindingType, to: BindingType): Type =
    if (tp.currentSymbol.isStatic) tp
    else tp.derivedSelect(subst(tp.prefix, from, to))

  private def substBound(tp: BoundType, from: BindingType, to: BindingType): Type =
    if (tp.binder eq from) tp.copyBoundType(to.asInstanceOf[tp.BT]) else tp

  final def subst1(tp: Type, from: Symbol, to: Type): Type = {
    tp match {
      case tp: NamedType => subst1Named(tp, from, to)
      case _: ThisType | _: BoundType => tp
      case _ => new Subst1Map(from, to).mapOver2(tp)
    }
  }

  private def subst1Named(tp: NamedType, from: Symbol, to: Type): Type = {
    val sym = tp.symbol
    if (sym eq from) return to
    if (sym.isStatic && !from.isStatic) tp
    else tp.derivedSelect(subst1(tp.prefix, from, to))
  }

  final def subst2(tp: Type, from1: Symbol, to1: Type, from2: Symbol, to2: Type): Type = tp match {
    case tp: NamedType => subst2Named(tp, from1, to1, from2, to2)
    case _: ThisType | _: BoundType => tp
    case _ => new Subst2Map(from1, to1, from2, to2).mapOver2(tp)
  }

  private def subst2Named(tp: NamedType, from1: Symbol, to1: Type, from2: Symbol, to2: Type): Type = {
    val sym = tp.symbol
    if (sym eq from1) return to1
    if (sym eq from2) return to2
    if (sym.isStatic && !from1.isStatic && !from2.isStatic) tp
    else tp.derivedSelect(subst2(tp.prefix, from1, to1, from2, to2))
  }

  final def subst(tp: Type, from: List[Symbol], to: List[Type]): Type = tp match {
    case tp: NamedType => substNamed(tp, from, to)
    case _: ThisType | _: BoundType => tp
    case _ => new SubstMap(from, to).mapOver2(tp)
  }

  private def substNamed(tp: NamedType, from: List[Symbol], to: List[Type]): Type = {
    val sym = tp.symbol
    var fs = from
    var ts = to
    while (fs.nonEmpty) {
      if (fs.head eq sym) return ts.head
      fs = fs.tail
      ts = ts.tail
    }
    if (sym.isStatic && !existsStatic(from)) tp
    else tp.derivedSelect(subst(tp.prefix, from, to))
  }

  final def substDealias(tp: Type, from: List[Symbol], to: List[Type]): Type = tp match {
    case tp: NamedType => substDealiasNamed(tp, from, to)
    case _: ThisType | _: BoundType => tp
    case _ => new SubstDealiasMap(from, to).mapOver2(tp)
  }

  private def substDealiasNamed(tp: NamedType, from: List[Symbol], to: List[Type]): Type = {
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
      tp.info match {
        case TypeAlias(alias) =>
          val alias1 = substDealias(alias, from, to)
          if (alias1 ne alias) return alias1
        case _ =>
      }
      tp.derivedSelect(substDealias(tp.prefix, from, to))
    }
  }

  final def substSym(tp: Type, from: List[Symbol], to: List[Symbol]): Type = tp match {
    case tp: NamedType => substSymNamed(tp, from, to)
    case tp: ThisType => substSymThis(tp, from, to)
    case _: BoundType => tp
    case _ => new SubstSymMap(from, to).mapOver2(tp)
  }

  private def substSymNamed(tp: NamedType, from: List[Symbol], to: List[Symbol]): Type = {
    val sym = tp.symbol
    var fs = from
    var ts = to
    while (fs.nonEmpty) {
      if (fs.head eq sym)
        return tp match {
          case tp: WithFixedSym => NamedType.withFixedSym(tp.prefix, ts.head)
          case _ => substSym(tp.prefix, from, to) select ts.head
        }
      fs = fs.tail
      ts = ts.tail
    }
    if (sym.isStatic && !existsStatic(from)) tp
    else tp.derivedSelect(substSym(tp.prefix, from, to))
  }

  private def substSymThis(tp: ThisType, from: List[Symbol], to: List[Symbol]): Type = {
    val sym = tp.cls
    var fs = from
    var ts = to
    while (fs.nonEmpty) {
      if (fs.head eq sym) return ts.head.asClass.thisType
      fs = fs.tail
      ts = ts.tail
    }
    tp
  }

  final def substThis(tp: Type, from: ClassSymbol, to: Type): Type = tp match {
    case tp: NamedType => substThisNamed(tp, from, to)
    case tp: ThisType => if (tp.cls eq from) to else tp
    case _: BoundType => tp
    case _ => new SubstThisMap(from, to).mapOver2(tp)
  }

  final def substThisNamed(tp: NamedType, from: ClassSymbol, to: Type): Type =
    if (tp.currentSymbol.isStaticOwner) tp
    else tp.derivedSelect(substThis(tp.prefix, from, to))

  final def substRecThis(tp: Type, from: Type, to: Type): Type = tp match {
    case tp: NamedType => substRecThisNamed(tp, from, to)
    case tp @ RecThis(binder) => if (binder eq from) to else tp
    case _: ThisType | _: BoundType => tp
    case _ => new SubstRecThisMap(from, to).mapOver2(tp)
  }

  final def substRecThisNamed(tp: NamedType, from: Type, to: Type): Type =
    if (tp.currentSymbol.isStatic) tp
    else tp.derivedSelect(substRecThis(tp.prefix, from, to))

  final def substParam(tp: Type, from: ParamRef, to: Type): Type = tp match {
    case tp: NamedType => substParamNamed(tp, from, to)
    case tp: BoundType => if (tp == from) to else tp
    case _: ThisType => tp
    case _ => new SubstParamMap(from, to).mapOver2(tp)
  }

  final def substParamNamed(tp: NamedType, from: ParamRef, to: Type): Type =
    if (tp.currentSymbol.isStatic) tp
    else tp.derivedSelect(substParam(tp.prefix, from, to))

  final def substParams(tp: Type, from: BindingType, to: List[Type]): Type = tp match {
    case tp: NamedType => substParamsNamed(tp, from, to)
    case tp: ParamRef => if (tp.binder == from) to(tp.paramNum) else tp
    case _: BoundType | _: ThisType => tp
    case _ => new SubstParamsMap(from, to).mapOver2(tp)
  }

  final def substParamsNamed(tp: NamedType, from: BindingType, to: List[Type]): Type =
    if (tp.currentSymbol.isStatic) tp
    else tp.derivedSelect(substParams(tp.prefix, from, to))

  private def existsStatic(syms: List[Symbol]): Boolean = syms match {
    case sym :: syms1 => sym.isStatic || existsStatic(syms1)
    case nil => false
  }

  final class SubstBindingMap(from: BindingType, to: BindingType) extends DeepTypeMap {
    def apply(tp: Type) = tp match {
      case tp: NamedType => substNamed(tp, from, to)
      case tp: BoundType => substBound(tp, from, to)
      case _: ThisType => tp
      case _ => mapOver2(tp)
    }

    // Specialize mapOver2 to get monomorphic dispatch for handling AppliedTypes
    override def mapOver2(tp: Type) = tp match {
      case tp: AppliedType =>
        def mapArgs(args: List[Type]): List[Type] = args match {
          case arg :: otherArgs =>
            val arg1 = this(arg)
            val otherArgs1 = mapArgs(otherArgs)
            if ((arg1 eq arg) && (otherArgs1 eq otherArgs)) args
            else arg1 :: otherArgs1
          case nil =>
            nil
        }
        derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args))
      case _ =>
        mapOver3(tp)
    }
  }

  final class Subst1Map(from: Symbol, to: Type) extends DeepTypeMap {
    def apply(tp: Type) = tp match {
      case tp: NamedType => subst1Named(tp, from, to)
      case _: ThisType | _: BoundType => tp
      case _ => mapOver2(tp)
    }

    // Specialize mapOver2 to get monomorphic dispatch for handling AppliedTypes
    override def mapOver2(tp: Type) = tp match {
      case tp: AppliedType =>
        def mapArgs(args: List[Type]): List[Type] = args match {
          case arg :: otherArgs =>
            val arg1 = this(arg)
            val otherArgs1 = mapArgs(otherArgs)
            if ((arg1 eq arg) && (otherArgs1 eq otherArgs)) args
            else arg1 :: otherArgs1
          case nil =>
            nil
        }
        derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args))
      case _ =>
        mapOver3(tp)
    }
  }

  final class Subst2Map(from1: Symbol, to1: Type, from2: Symbol, to2: Type) extends DeepTypeMap {
    def apply(tp: Type) = tp match {
      case tp: NamedType => subst2Named(tp, from1, to1, from2, to2)
      case _: ThisType | _: BoundType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstMap(from: List[Symbol], to: List[Type]) extends DeepTypeMap {
    def apply(tp: Type) = tp match {
      case tp: NamedType => substNamed(tp, from, to)
      case _: ThisType | _: BoundType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstDealiasMap(from: List[Symbol], to: List[Type]) extends DeepTypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => substDealiasNamed(tp, from, to)
      case _: ThisType | _: BoundType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstSymMap(from: List[Symbol], to: List[Symbol]) extends DeepTypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => substSymNamed(tp, from, to)
      case tp: ThisType => substSymThis(tp, from, to)
      case _: BoundType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstThisMap(from: ClassSymbol, to: Type) extends DeepTypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => substThisNamed(tp, from, to)
      case tp: ThisType => if (tp.cls eq from) to else tp
      case _: BoundType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstRecThisMap(from: Type, to: Type) extends DeepTypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => substRecThisNamed(tp, from, to)
      case tp @ RecThis(binder) => if (binder eq from) to else tp
      case _: ThisType | _: BoundType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstParamMap(from: ParamRef, to: Type) extends DeepTypeMap {
    def apply(tp: Type) = tp match {
      case tp: NamedType => substParamNamed(tp, from, to)
      case tp: BoundType => if (tp == from) to else tp
      case _: ThisType => tp
      case _ => mapOver2(tp)
    }
  }

  final class SubstParamsMap(from: BindingType, to: List[Type]) extends DeepTypeMap {
    def apply(tp: Type) = tp match {
      case tp: NamedType => substParamsNamed(tp, from, to)
      case tp: ParamRef => if (tp.binder == from) to(tp.paramNum) else tp
      case _: BoundType | _: ThisType => tp
      case _ => mapOver2(tp)
    }
  }
}
