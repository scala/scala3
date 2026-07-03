package dotty.tools.dotc
package core

import Types.*, Symbols.*, Contexts.*, Decorators.i
import cc.Capabilities.{Capability, ResultCap}
import dotty.tools.dotc.util.EqHashMap

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
object Substituters:
  private inline val LargeSubstThreshold = 32

  final def subst[BT <: BindingType](tp: Type, from: BT, to: BT, theMap: SubstBindingMap[BT] | Null)(using Context): Type =
    tp match {
      case tp: BoundType =>
        if (tp.binder eq from) tp.copyBoundType(to.asInstanceOf[tp.BT]) else tp
      case tp: NamedType =>
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(subst(tp.prefix, from, to, theMap))
      case _: ThisType =>
        tp
      case tp: AppliedType =>
        tp.map(subst(_, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstBindingMap(from, to))
          .mapOver(tp)
    }

  final def subst1(tp: Type, from: Symbol, to: Type, theMap: Subst1Map | Null)(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym eq from) return to
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(subst1(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new Subst1Map(from, to))
          .mapOver(tp)
    }

  final def subst2(tp: Type, from1: Symbol, to1: Type, from2: Symbol, to2: Type, theMap: Subst2Map  | Null)(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym eq from1) return to1
        if (sym eq from2) return to2
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(subst2(tp.prefix, from1, to1, from2, to2, theMap))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new Subst2Map(from1, to1, from2, to2))
          .mapOver(tp)
    }

  final def subst(tp: Type, from: Vector[Symbol], to: Vector[Type], theMap: SubstMap | Null)(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        val len = math.min(from.length, to.length)
        var idx = 0
        while (idx < len) {
          if (from(idx) eq sym) return to(idx)
          idx += 1
        }
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(subst(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstMap(from, to))
          .mapOver(tp)
    }

  final def substSym(tp: Type, from: Vector[Symbol], to: Vector[Symbol], theMap: SubstSymMap | Null)(using Context): Type =
    if theMap == null then
      from.length match
        case 0 => return tp
        case 1 => return substSym1(tp, from(0), to(0), null)
        case 2 => return substSym2(tp, from(0), to(0), from(1), to(1), null)
        case _ =>
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        var idx = 0
        while (idx < from.length) {
          if (from(idx) eq sym)
            return substSym(tp.prefix, from, to, theMap).select(to(idx))
          idx += 1
        }
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substSym(tp.prefix, from, to, theMap))
      case tp: ThisType =>
        val sym = tp.cls
        var idx = 0
        while (idx < from.length) {
          if (from(idx) eq sym) return to(idx).asClass.thisType
          idx += 1
        }
        tp
      case _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstSymMap(from, to))
          .mapOver(tp)
    }

  private def largeSubstLookup(from: Array[Symbol], to: Array[Symbol]): EqHashMap[Symbol, Symbol] | Null =
    if from.length <= LargeSubstThreshold then null
    else
      val lookup = new EqHashMap[Symbol, Symbol](from.length * 2)
      var i = 0
      while i < from.length do
        if lookup.lookup(from(i)) == null then lookup.update(from(i), to(i))
        i += 1
      lookup

  private def substSymLookup(sym: Symbol, from: Array[Symbol], to: Array[Symbol], lookup: EqHashMap[Symbol, Symbol] | Null): Symbol | Null =
    if lookup != null then lookup.lookup(sym)
    else
      var idx = 0
      while idx < from.length do
        if from(idx) eq sym then return to(idx)
        idx += 1
      null

  private final def substSymArray(
      tp: Type,
      from: Array[Symbol],
      to: Array[Symbol],
      lookup: EqHashMap[Symbol, Symbol] | Null,
      theMap: TypeMap
  )(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym1 = substSymLookup(tp.symbol, from, to, lookup)
        if sym1 != null then
          return substSymArray(tp.prefix, from, to, lookup, theMap).select(sym1)
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substSymArray(tp.prefix, from, to, lookup, theMap))
      case tp: ThisType =>
        val sym1 = substSymLookup(tp.cls, from, to, lookup)
        if sym1 == null then tp else sym1.asClass.thisType
      case _: BoundType =>
        tp
      case _ =>
        theMap.mapOver(tp)
    }

  final class SubstSymData(fromVector: Vector[Symbol], toVector: Vector[Symbol]):
    assert(fromVector.length == toVector.length, s"mismatched substitution: $fromVector --> $toVector")
    private val from = fromVector.toArray
    private val to = toVector.toArray
    private val lookup = largeSubstLookup(from, to)

    private[core] def subst(tp: Type, theMap: TypeMap)(using Context): Type =
      substSymArray(tp, from, to, lookup, theMap)

    def substImportAware(tp: Type)(using Context): Type =
      // Import references are only shielded while the traversal descends through
      // import prefixes; once substitution enters an ordinary type it proceeds
      // unguarded through `plainMap`, matching `TreeTypeMap.mapType`'s original
      // semantics (and the arity-1/2 paths, whose maps are import-unaware).
      object plainMap extends DeepTypeMap:
        def apply(tp: Type): Type = subst(tp, this)
      val substMap = new DeepTypeMap:
        def apply(tp: Type): Type = tp match
          case tp: TermRef if tp.symbol.isImport => mapOver(tp)
          case tp => subst(tp, plainMap)
      substMap(tp)

  final def substSym1(tp: Type, from: Symbol, to: Symbol, theMap: SubstSym1Map | Null)(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (from eq sym)
          return substSym1(tp.prefix, from, to, theMap).select(to)
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substSym1(tp.prefix, from, to, theMap))
      case tp: ThisType =>
        if (from eq tp.cls) to.asClass.thisType else tp
      case _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstSym1Map(from, to))
          .mapOver(tp)
    }

  final def substSym2(
      tp: Type,
      from1: Symbol,
      to1: Symbol,
      from2: Symbol,
      to2: Symbol,
      theMap: SubstSym2Map | Null
  )(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (from1 eq sym)
          return substSym2(tp.prefix, from1, to1, from2, to2, theMap).select(to1)
        if (from2 eq sym)
          return substSym2(tp.prefix, from1, to1, from2, to2, theMap).select(to2)
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substSym2(tp.prefix, from1, to1, from2, to2, theMap))
      case tp: ThisType =>
        val sym = tp.cls
        if (from1 eq sym) to1.asClass.thisType
        else if (from2 eq sym) to2.asClass.thisType
        else tp
      case _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstSym2Map(from1, to1, from2, to2))
          .mapOver(tp)
    }

  final def substThis(tp: Type, from: ClassSymbol, to: Type, theMap: SubstThisMap | Null)(using Context): Type =
    tp match {
      case tp: ThisType =>
        if (tp.cls eq from) to else tp
      case tp: NamedType =>
        if (tp.currentSymbol.isStaticOwner || (tp.prefix `eq` NoPrefix)) tp
        else tp.derivedSelect(substThis(tp.prefix, from, to, theMap))
      case _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstThisMap(from, to))
          .mapOver(tp)
    }

  final def substRecThis(tp: Type, from: Type, to: Type, theMap: SubstRecThisMap | Null)(using Context): Type =
    tp match {
      case tp @ RecThis(binder) =>
        if (binder eq from) to else tp
      case tp: NamedType =>
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substRecThis(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstRecThisMap(from, to))
          .mapOver(tp)
    }

  final def substParam(tp: Type, from: ParamRef, to: Type, theMap: SubstParamMap | Null)(using Context): Type =
    tp match {
      case tp: BoundType =>
        if (tp == from) to else tp
      case tp: NamedType =>
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substParam(tp.prefix, from, to, theMap))
      case _: ThisType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstParamMap(from, to))
          .mapOver(tp)
    }

  final def substParams(tp: Type, from: BindingType, to: Vector[Type], theMap: SubstParamsMap | Null)(using Context): Type =
    tp match {
      case tp: ParamRef =>
        if (tp.binder == from) to(tp.paramNum) else tp
      case tp: NamedType =>
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substParams(tp.prefix, from, to, theMap))
      case _: ThisType =>
        tp
      case tp: AppliedType =>
        tp.map(substParams(_, from, to, theMap))
      case _ =>
        (if (theMap != null) theMap else new SubstParamsMap(from, to))
          .mapOver(tp)
    }

  final class SubstBindingMap[BT <: BindingType](val from: BT, val to: BT)(using Context) extends DeepTypeMap, BiTypeMap {
    def apply(tp: Type): Type = subst(tp, from, to, this)(using mapCtx)
    override def mapCapability(c: Capability) = c match
      case c @ ResultCap(binder) if binder eq from =>
        c.derivedResult(to.asInstanceOf[MethodicType])
      case _ =>
        super.mapCapability(c)

    override def fuse(next: BiTypeMap)(using Context) = next match
      case next: SubstBindingMap[_] =>
        if next.from eq to then Some(SubstBindingMap(from, next.to))
        else Some(SubstBindingsMap(Array(from, next.from), Array(to, next.to)))
      case _ => None

    override def summarize(using Context) = i"SubstBinding[$from --> $to]"

    def inverse = SubstBindingMap(to, from)
  }

  final class SubstBindingsMap(val from: Array[BindingType], val to: Array[BindingType])(using Context) extends DeepTypeMap, BiTypeMap {

    def apply(tp: Type): Type = tp match
      case tp: BoundType =>
        var i = 0
        while i < from.length && (from(i) ne tp.binder) do i += 1
        if i < from.length then tp.copyBoundType(to(i).asInstanceOf[tp.BT]) else tp
      case _ =>
        mapOver(tp)

    override def mapCapability(c: Capability) = c match
      case c @ ResultCap(binder: MethodType) =>
        var i = 0
        while i < from.length && (from(i) ne binder) do i += 1
        if i < from.length then c.derivedResult(to(i).asInstanceOf[MethodType]) else c
      case _ =>
        super.mapCapability(c)

    override def fuse(next: BiTypeMap)(using Context) = next match
      case next: SubstBindingMap[_] =>
        var i = 0
        while i < from.length && (to(i) ne next.from) do i += 1
        if i < from.length then Some(SubstBindingsMap(from, to.updated(i, next.to)))
        else Some(SubstBindingsMap(from :+ next.from, to :+ next.to))
      case _ => None

    def inverse = SubstBindingsMap(to, from)
  }

  final class Subst1Map(from: Symbol, to: Type)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = subst1(tp, from, to, this)(using mapCtx)
  }

  final class Subst2Map(from1: Symbol, to1: Type, from2: Symbol, to2: Type)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = subst2(tp, from1, to1, from2, to2, this)(using mapCtx)
  }

  final class SubstMap(from: Vector[Symbol], to: Vector[Type])(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = subst(tp, from, to, this)(using mapCtx)
  }

  final class SubstSymMap(fromVector: Vector[Symbol], toVector: Vector[Symbol])(using Context) extends DeepTypeMap {
    private val data = SubstSymData(fromVector, toVector)
    def apply(tp: Type): Type = data.subst(tp, this)(using mapCtx)
    def inverse = SubstSymMap(toVector, fromVector) // implicitly requires that `to` contains no duplicates.
  }

  final class SubstSym1Map(from: Symbol, to: Symbol)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substSym1(tp, from, to, this)(using mapCtx)
  }

  final class SubstSym2Map(from1: Symbol, to1: Symbol, from2: Symbol, to2: Symbol)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substSym2(tp, from1, to1, from2, to2, this)(using mapCtx)
  }

  final class SubstThisMap(from: ClassSymbol, to: Type)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substThis(tp, from, to, this)(using mapCtx)
  }

  final class SubstRecThisMap(from: Type, to: Type)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substRecThis(tp, from, to, this)(using mapCtx)
  }

  final class SubstParamMap(from: ParamRef, to: Type)(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substParam(tp, from, to, this)(using mapCtx)
  }

  final class SubstParamsMap(from: BindingType, to: Vector[Type])(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substParams(tp, from, to, this)(using mapCtx)
  }

  /** An approximating substitution that can handle wildcards in the `to` list */
  final class SubstApproxMap(from: Vector[Symbol], to: Vector[Type])(using Context) extends ApproximatingTypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        val len = math.min(from.length, to.length)
        var idx = 0
        while (idx < len) {
          if (from(idx) eq sym)
            return to(idx) match {
              case TypeBounds(lo, hi) => range(lo, hi)
              case tp1 => tp1
            }
          idx += 1
        }
        if (tp.prefix `eq` NoPrefix) tp else derivedSelect(tp, apply(tp.prefix))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        mapOver(tp)
    }
  }
end Substituters
