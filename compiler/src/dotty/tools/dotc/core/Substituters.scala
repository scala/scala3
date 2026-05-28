package dotty.tools.dotc
package core

import Types.*, Symbols.*, Contexts.*, Decorators.i
import cc.Capabilities.{Capability, ResultCap}

/** Substitution operations on types. See the corresponding `subst` and
 *  `substThis` methods on class Type for an explanation.
 */
object Substituters:

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

  final def subst(tp: Type, from: List[Symbol], to: List[Type], theMap: SubstMap | Null)(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if theMap != null then
          val lookup = theMap.lookup
          if lookup != null then
            val replacement = lookup.get(sym)
            if replacement != null then return replacement.nn
            if tp.prefix `eq` NoPrefix then return tp
            else return tp.derivedSelect(subst(tp.prefix, from, to, theMap))
        var fs = from
        var ts = to
        while (fs.nonEmpty && ts.nonEmpty) {
          if (fs.head eq sym) return ts.head
          fs = fs.tail
          ts = ts.tail
        }
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(subst(tp.prefix, from, to, theMap))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstMap(from, to))
          .mapOver(tp)
    }

  final def substSym(tp: Type, from: List[Symbol], to: List[Symbol], theMap: SubstSymMap | Null)(using Context): Type =
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if theMap != null then
          val lookup = theMap.lookup
          if lookup != null then
            val replacement = lookup.get(sym)
            if replacement != null then
              val prefix = tp.prefix
              return (if prefix `eq` NoPrefix then NoPrefix else theMap.substPrefix(prefix, from, to)).select(replacement.nn)
            if tp.prefix `eq` NoPrefix then return tp
            else return tp.derivedSelect(theMap.substPrefix(tp.prefix, from, to))
        var fs = from
        var ts = to
        while (fs.nonEmpty) {
          if (fs.head eq sym)
            val prefix = tp.prefix
            return (if prefix `eq` NoPrefix then NoPrefix
                    else if theMap != null then theMap.substPrefix(prefix, from, to)
                    else substSym(prefix, from, to, theMap)
                   ).select(ts.head)
          fs = fs.tail
          ts = ts.tail
        }
        if (tp.prefix `eq` NoPrefix) tp
        else if theMap != null then tp.derivedSelect(theMap.substPrefix(tp.prefix, from, to))
        else tp.derivedSelect(substSym(tp.prefix, from, to, theMap))
      case tp: ThisType =>
        val sym = tp.cls
        if theMap != null then
          val lookup = theMap.lookup
          if lookup != null then
            val replacement = lookup.get(sym)
            if replacement != null then return replacement.nn.asClass.thisType
            return tp
        var fs = from
        var ts = to
        while (fs.nonEmpty) {
          if (fs.head eq sym) return ts.head.asClass.thisType
          fs = fs.tail
          ts = ts.tail
        }
        tp
      case _: BoundType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstSymMap(from, to))
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
        // `eq` short-circuit avoids the wrapper `Objects.equals` call on the
        // common reference-equal case.
        if ((tp eq from) || tp == from) to else tp
      case tp: NamedType =>
        if (tp.prefix `eq` NoPrefix) tp
        else tp.derivedSelect(substParam(tp.prefix, from, to, theMap))
      case _: ThisType =>
        tp
      case _ =>
        (if (theMap != null) theMap else new SubstParamMap(from, to))
          .mapOver(tp)
    }

  final def substParams(tp: Type, from: BindingType, to: List[Type], theMap: SubstParamsMap | Null)(using Context): Type =
    tp match {
      case tp: ParamRef =>
        // `eq` short-circuit avoids the wrapper `Objects.equals` call on the
        // common case where ParamRefs are created via `binder.paramRefs(n)`
        // with cached results, so the binder is reference-equal.
        if ((tp.binder eq from) || tp.binder == from) to(tp.paramNum) else tp
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

  final class SubstMap(from: List[Symbol], to: List[Type])(using Context) extends DeepTypeMap {
    private val useLookup = from.lengthCompare(4) >= 0
    private var lookupCache: java.util.IdentityHashMap[Symbol, Type] | Null = null

    private[Substituters] def lookup: java.util.IdentityHashMap[Symbol, Type] | Null =
      if !useLookup then null
      else
        var m = lookupCache
        if m == null then
          m = new java.util.IdentityHashMap[Symbol, Type]
          var fs = from
          var ts = to
          while fs.nonEmpty && ts.nonEmpty do
            // first-occurrence wins; matches the linear-scan semantics
            if !m.containsKey(fs.head) then m.put(fs.head, ts.head)
            fs = fs.tail
            ts = ts.tail
          lookupCache = m
        m

    def apply(tp: Type): Type = subst(tp, from, to, this)(using mapCtx)
  }

  final class SubstSymMap(from: List[Symbol], to: List[Symbol])(using Context) extends DeepTypeMap {
    // Above this length the per-NamedType linear scan of `from` becomes
    // measurable in deep inlining; cache an identity-keyed lookup once.
    private[Substituters] val lookup: java.util.IdentityHashMap[Symbol, Symbol] | Null =
      if from.lengthCompare(4) >= 0 then
        val m = new java.util.IdentityHashMap[Symbol, Symbol]
        var fs = from
        var ts = to
        while fs.nonEmpty && ts.nonEmpty do
          // first-occurrence wins; matches the linear-scan semantics
          if !m.containsKey(fs.head) then m.put(fs.head, ts.head)
          fs = fs.tail
          ts = ts.tail
        m
      else null

    // One-slot MRU keyed by reference equality on the input prefix:
    // sibling NamedTypes selecting different names from the same prefix
    // (e.g. `outer.this.x` / `outer.this.y`) reuse the substituted prefix
    // instead of re-walking it. The result is a pure function of the input
    // prefix and the (from, to) lists, all immutable for this instance.
    private var prefixIn: Type | Null = null
    private var prefixOut: Type | Null = null

    private[Substituters] def substPrefix(prefix: Type, from: List[Symbol], to: List[Symbol])(using Context): Type =
      val cached = prefixIn
      if cached != null && (prefix eq cached) then prefixOut.nn
      else
        val out = substSym(prefix, from, to, this)
        prefixIn = prefix
        prefixOut = out
        out

    def apply(tp: Type): Type = substSym(tp, from, to, this)(using mapCtx)
    def inverse = SubstSymMap(to, from) // implicitly requires that `to` contains no duplicates.
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

  final class SubstParamsMap(from: BindingType, to: List[Type])(using Context) extends DeepTypeMap {
    def apply(tp: Type): Type = substParams(tp, from, to, this)(using mapCtx)
  }

  /** An approximating substitution that can handle wildcards in the `to` list */
  final class SubstApproxMap(from: List[Symbol], to: List[Type])(using Context) extends ApproximatingTypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        var fs = from
        var ts = to
        while (fs.nonEmpty && ts.nonEmpty) {
          if (fs.head eq sym)
            return ts.head match {
              case TypeBounds(lo, hi) => range(lo, hi)
              case tp1 => tp1
            }
          fs = fs.tail
          ts = ts.tail
        }
        if (tp.prefix `eq` NoPrefix) tp else derivedSelect(tp, apply(tp.prefix))
      case _: ThisType | _: BoundType =>
        tp
      case _ =>
        mapOver(tp)
    }
  }
end Substituters
