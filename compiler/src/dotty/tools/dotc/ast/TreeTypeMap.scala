package dotty.tools
package dotc
package ast

import core.*
import Types.*, Contexts.*, Flags.*
import Symbols.*, Annotations.*, Trees.*, Symbols.*, Constants.Constant
import Decorators.*
import config.Config
import java.util.IdentityHashMap
import scala.collection.mutable
import scala.annotation.internal.sharable

object TreeTypeMap:
  /** Shared identity sentinel for the `treeMap` parameter. Using a single
   *  reference (instead of eta-expanding `identity` per construction) lets
   *  `transform` skip the megamorphic no-op `treeMap(tree)` call for the ~51%
   *  of TreeTypeMaps that carry no custom treeMap. Sound because `apply(t) eq t`.
   */
  @sharable val IdentityTreeMap: tpd.Tree => tpd.Tree = t => t


/** A map that applies three functions and a substitution together to a tree and
 *  makes sure they are coordinated so that the result is well-typed. The functions are
 *  @param typeMap   A function from Type to Type that gets applied to the
 *                   type of every tree node and to all locally defined symbols,
 *                   followed by the substitution [substFrom := substTo].
 *  @param treeMap   A transformer that translates all encountered subtrees in
 *                   prefix traversal orders
 *  @param oldOwners Previous owners. If a top-level local symbol in the mapped tree
 *                   has one of these as an owner, the owner is replaced by the corresponding
 *                   symbol in `newOwners`.
 *  @param newOwners New owners, replacing previous owners.
 *  @param substFrom The symbols that need to be substituted.
 *  @param substTo   The substitution targets.
 *
 *  The reason the substitution is broken out from the rest of the type map is
 *  that all symbols have to be substituted at the same time. If we do not do this,
 *  we risk data races on named types. Example: Say we have `outer#1.inner#2` and we
 *  have two substitutions S1 = [outer#1 := outer#3], S2 = [inner#2 := inner#4] where
 *  hashtags precede symbol ids. If we do S1 first, we get outer#2.inner#3. If we then
 *  do S2 we get outer#2.inner#4. But that means that the named type outer#2.inner
 *  gets two different denotations in the same period. Hence, if -Yno-double-bindings is
 *  set, we would get a data race assertion error.
 */
class TreeTypeMap(
  val typeMap: Type => Type = IdentityTypeMap,
  val treeMap: tpd.Tree => tpd.Tree = TreeTypeMap.IdentityTreeMap,
  val oldOwners: List[Symbol] = Nil,
  val newOwners: List[Symbol] = Nil,
  val substFrom: List[Symbol] = Nil,
  val substTo: List[Symbol] = Nil,
  cpy: tpd.TreeCopier = tpd.cpy,
  cacheTypeMap: Boolean = false)(using Context) extends tpd.TreeMap(cpy) {
  import tpd.*

  def copy(
      typeMap: Type => Type,
      treeMap: Tree => Tree,
      oldOwners: List[Symbol],
      newOwners: List[Symbol],
      substFrom: List[Symbol],
      substTo: List[Symbol])(using Context): TreeTypeMap =
    new TreeTypeMap(typeMap, treeMap, oldOwners, newOwners, substFrom, substTo, cacheTypeMap = cacheTypeMap)

  private var mySubstFromLookup: IdentityHashMap[Symbol, java.lang.Boolean] | Null = null
  private var mySubstToLookup: IdentityHashMap[Symbol, java.lang.Boolean] | Null = null
  private var myOldOwnerLookup: IdentityHashMap[Symbol, java.lang.Boolean] | Null = null
  private var myNewOwnerLookup: IdentityHashMap[Symbol, java.lang.Boolean] | Null = null

  private def symbolSetLookup(
      syms: List[Symbol],
      lookup: IdentityHashMap[Symbol, java.lang.Boolean] | Null): IdentityHashMap[Symbol, java.lang.Boolean] =
    if lookup != null then lookup
    else
      val fresh = new IdentityHashMap[Symbol, java.lang.Boolean](syms.length)
      var xs = syms
      while xs.nonEmpty do
        fresh.put(xs.head, java.lang.Boolean.TRUE)
        xs = xs.tail
      fresh

  private def substFromLookup: IdentityHashMap[Symbol, java.lang.Boolean] =
    val lookup = symbolSetLookup(substFrom, mySubstFromLookup)
    mySubstFromLookup = lookup
    lookup

  private def substToLookup: IdentityHashMap[Symbol, java.lang.Boolean] =
    val lookup = symbolSetLookup(substTo, mySubstToLookup)
    mySubstToLookup = lookup
    lookup

  private def oldOwnerLookup: IdentityHashMap[Symbol, java.lang.Boolean] =
    val lookup = symbolSetLookup(oldOwners, myOldOwnerLookup)
    myOldOwnerLookup = lookup
    lookup

  private def newOwnerLookup: IdentityHashMap[Symbol, java.lang.Boolean] =
    val lookup = symbolSetLookup(newOwners, myNewOwnerLookup)
    myNewOwnerLookup = lookup
    lookup

  private def substToContains(sym: Symbol): Boolean =
    var xs = substTo
    var remaining = 3
    while xs.nonEmpty && remaining > 0 do
      if sym eq xs.head then return true
      xs = xs.tail
      remaining -= 1
    xs.nonEmpty && substToLookup.get(sym) != null

  private def substFromContains(sym: Symbol): Boolean =
    var xs = substFrom
    var remaining = 3
    while xs.nonEmpty && remaining > 0 do
      if sym eq xs.head then return true
      xs = xs.tail
      remaining -= 1
    xs.nonEmpty && substFromLookup.get(sym) != null

  private def oldOwnerContains(sym: Symbol): Boolean =
    var xs = oldOwners
    var remaining = 3
    while xs.nonEmpty && remaining > 0 do
      if sym eq xs.head then return true
      xs = xs.tail
      remaining -= 1
    xs.nonEmpty && oldOwnerLookup.get(sym) != null

  private def newOwnerContains(sym: Symbol): Boolean =
    var xs = newOwners
    var remaining = 3
    while xs.nonEmpty && remaining > 0 do
      if sym eq xs.head then return true
      xs = xs.tail
      remaining -= 1
    xs.nonEmpty && newOwnerLookup.get(sym) != null

  private def hasAnyInSubstTo(syms: List[Symbol]): Boolean =
    var xs = syms
    while xs.nonEmpty do
      if substToContains(xs.head) then return true
      xs = xs.tail
    false

  private def hasAnyInSubstFrom(syms: List[Symbol]): Boolean =
    var xs = syms
    while xs.nonEmpty do
      if substFromContains(xs.head) then return true
      xs = xs.tail
    false

  private def hasAnyInNewOwners(syms: List[Symbol]): Boolean =
    var xs = syms
    while xs.nonEmpty do
      if newOwnerContains(xs.head) then return true
      xs = xs.tail
    false

  private def hasAnyInOldOwners(syms: List[Symbol]): Boolean =
    var xs = syms
    while xs.nonEmpty do
      if oldOwnerContains(xs.head) then return true
      xs = xs.tail
    false

  private def hasSubstitutionPair(from: Symbol, to: Symbol): Boolean =
    var fs = substFrom
    var ts = substTo
    while fs.nonEmpty && ts.nonEmpty do
      if (fs.head eq from) && (ts.head eq to) then return true
      fs = fs.tail
      ts = ts.tail
    false

  // `mapOwnerThis` only substitutes ThisType prefixes whose `cls` matches a
  // ClassSymbol in `oldOwners`; entries that aren't ClassSymbols are stepped
  // over without effect. Class owner remaps introduced by `withSubstitution`
  // are already covered by `substSym`, so only class owners without a matching
  // substitution pair need the extra TypeMap walk.
  private val hasUncoveredOwnerClass: Boolean =
    var olds = oldOwners
    var news = newOwners
    var found = false
    while !found && (olds ne Nil) do
      val old = olds.head
      val next = news.head
      if old.isClass && !hasSubstitutionPair(old, next) then found = true
      olds = olds.tail
      news = news.tail
    found

  /** If `sym` is one of `oldOwners`, replace by corresponding symbol in `newOwners` */
  def mapOwner(sym: Symbol): Symbol =
    var olds = oldOwners
    var news = newOwners
    while olds.nonEmpty && news.nonEmpty do
      if sym eq olds.head then return news.head
      olds = olds.tail
      news = news.tail
    sym

  /** Is `sym` one of the owner keys remapped by this TreeTypeMap? */
  def mapsOwner(sym: Symbol): Boolean = oldOwnerContains(sym)

  /** If `sym` is one of `substFrom`, replace by the corresponding symbol in `substTo`. */
  def mapSubstitution(sym: Symbol): Symbol =
    var from = substFrom
    var to = substTo
    while from.nonEmpty && to.nonEmpty do
      if sym eq from.head then return to.head
      from = from.tail
      to = to.tail
    sym

  /** Replace occurrences of `This(oldOwner)` in some prefix of a type
   *  by the corresponding `This(newOwner)`.
   *
   *  Allocated only when `hasUncoveredOwnerClass` (the sole condition under which
   *  `computeMapType` calls it); otherwise it would be ~110K dead anonymous-TypeMap
   *  allocations per compile. A construction-time conditional `val` (not a `lazy val`)
   *  so the captured context stays the construction-time `mapCtx`.
   */
  private val mapOwnerThis: TypeMap | Null =
    if !hasUncoveredOwnerClass then null
    else new TypeMap {
      private def mapPrefix(from: List[Symbol], to: List[Symbol], tp: Type): Type = from match {
        case Nil => tp
        case (cls: ClassSymbol) :: from1 => mapPrefix(from1, to.tail, tp.substThis(cls, to.head.thisType))
        case _ :: from1 => mapPrefix(from1, to.tail, tp)
      }
      def apply(tp: Type): Type = tp match {
        case tp: NamedType => tp.derivedSelect(mapPrefix(oldOwners, newOwners, tp.prefix))
        case _ => mapOver(tp)
      }
    }

  // Cached eagerly for non-empty substitutions so mapType's hot substitution
  // path can reuse the SubstSymMap without paying lazy-val accessor checks.
  private val cachedSubstSymMap: Substituters.SubstSymMap | Null =
    if substFrom.nonEmpty then new Substituters.SubstSymMap(substFrom, substTo) else null

  private val substMap: TypeMap | Null =
    if substFrom.nonEmpty then
      new TypeMap():
        def apply(tp: Type): Type = tp match
          case tp: TermRef if tp.symbol.isImport => mapOver(tp)
          case tp => cachedSubstSymMap.nn(tp)
    else null

  // Identity-keyed cache for `mapType`: Types are uniqued, so the same Type
  // input always produces the same mapped output for a given TreeTypeMap.
  // The same `tree.tpe` is re-asked many times when an inliner's TreeTypeMap
  // walks a tree. Lazily allocated on first non-trivial mapType call so
  // trivial maps (no substFrom + no owner remap) pay no overhead.
  private var mapTypeMruKey: Type | Null = null
  private var mapTypeMruValue: Type | Null = null
  private var myMapTypeCache: util.EqHashMap[Type, Type] | Null = null
  // `mapType(tp) eq tp` for EVERY input: identity typeMap leaves `tp1 eq tp`,
  // empty substFrom leaves `tp2 eq tp1`, and no uncovered owner class means
  // `mapOwnerThis` is never applied. This is the owner-only `changeOwner` map
  // class (13,857 maps / 120,814 mapType calls on the corpus), which today pays
  // the full MRU+EqHashMap+computeMapType machinery for a guaranteed no-op.
  // The `!cacheTypeMap && !Config.checkTreesConsistent` guard keeps the bypass
  // byte-identical under the debug flags (where it is a no-op either way).
  private val typeIsIdentity: Boolean =
    !cacheTypeMap
      && (typeMap eq IdentityTypeMap)
      && substFrom.isEmpty
      && !hasUncoveredOwnerClass
      && !Config.checkTreesConsistent
  // Cache only when there is real per-type work: a substitution walk, an
  // uncovered owner-class ThisType remap, or a custom typeMap. The
  // `(typeMap ne IdentityTypeMap)` disjunct is REQUIRED to keep caching the
  // inliner's top-level DeepTypeMap (custom typeMap, empty subst, non-class
  // owner); without it that map re-runs `Inliner$$anon$8.apply`/`mapOver` per
  // repeated type. Owner-only no-class maps (now `typeIsIdentity`) are routed to
  // the uncached arm and never allocate an EqHashMap.
  private val shouldCacheMapType =
    cacheTypeMap || substFrom.nonEmpty || hasUncoveredOwnerClass || (typeMap ne IdentityTypeMap)
  // Exact identity-type maps with no symbol or owner state only need the
  // caller-provided treeMap; derived maps with local symbols recompute this.
  private val treeMapOnly =
    !cacheTypeMap
      && (typeMap eq IdentityTypeMap)
      && oldOwners.isEmpty
      && newOwners.isEmpty
      && substFrom.isEmpty
      && substTo.isEmpty
      && !Config.checkTreesConsistent

  def mapType(tp: Type): Type =
    // For owner-only `changeOwner` maps the result is provably `tp` (see
    // `typeIsIdentity`), so return it directly without touching the cache,
    // covering the ~21K direct mapType calls (Literal/Template/withMappedSymsGeneral)
    // that don't go through `withMappedType`. This branch is predictable-false for
    // caching maps (`typeIsIdentity` and `shouldCacheMapType` are mutually exclusive).
    if typeIsIdentity then tp
    // Cache only when we actually do non-trivial work (substSym walk and/or
    // ownerThis walk), or when a deterministic custom typeMap explicitly opts in.
    // Pure `typeMap(tp)` calls stay uncached by default because they may depend on
    // mutable state or have their own caching.
    else if shouldCacheMapType then
      if mapTypeMruKey eq tp then return mapTypeMruValue.nn
      var cache = myMapTypeCache
      if cache == null then
        cache = util.EqHashMap[Type, Type]()
        myMapTypeCache = cache
      val res = cache.getOrElseUpdate(tp, computeMapType(tp))
      mapTypeMruKey = tp
      mapTypeMruValue = res
      res
    else computeMapType(tp)
  end mapType

  private def computeMapType(tp: Type): Type =
    val tp1 = if typeMap eq IdentityTypeMap then tp else typeMap(tp)
    val tp2 = if substFrom.isEmpty then tp1 else substMap.nn(tp1)
    // Skip the owner-this TypeMap when there is no owner-only class remap left
    // after the symbol substitution above.
    if !hasUncoveredOwnerClass then tp2 else mapOwnerThis.nn(tp2)
  end computeMapType

  private def updateDecls(prevStats: List[Tree], newStats: List[Tree]): Unit =
    if (prevStats.isEmpty) assert(newStats.isEmpty)
    else {
      prevStats.head match {
        case pdef: MemberDef =>
          val prevSym = pdef.symbol
          val newSym = newStats.head.symbol
          val newCls = newSym.owner.asClass
          if (prevSym != newSym) newCls.replace(prevSym, newSym)
        case _ =>
      }
      updateDecls(prevStats.tail, newStats.tail)
    }

  def transformInlined(tree: Inlined)(using Context): Tree =
    val Inlined(call, bindings, expanded) = tree
    val (tmap1, bindings1) = transformDefs(bindings)
    val expanded1 = tmap1.transform(expanded)
    cpy.Inlined(tree)(call, bindings1, expanded1)

  private def withMappedType(tree: Tree)(using Context): Tree =
    val tpe = tree.tpe
    // For owner-only `changeOwner` maps `mapType(tpe) eq tpe` always, so skip the
    // `mapType` machinery entirely (the flag already includes
    // `!Config.checkTreesConsistent`). Preserve the ErrorType branch byte-identically:
    // `withType(tpe)` hits the `myTpe eq tpe` fast path and returns the same tree.
    if typeIsIdentity then
      if tpe.isInstanceOf[ErrorType] then tree.withType(tpe) else tree
    else
      val tpe1 = mapType(tpe)
      if (tpe1 eq tpe) && !tpe1.isInstanceOf[ErrorType] && !Config.checkTreesConsistent then tree
      else tree.withType(tpe1)

  private def sourceCtx(tree: Tree)(using Context): Context =
    val source = tree.source
    if source.exists && (source ne ctx.source) then ctx.withSource(source) else ctx

  protected def noteTransformedMemberDef(member: MemberDef)(using Context): Unit = ()

  override def transform(tree: Tree)(using Context): Tree =
    (if treeMap eq TreeTypeMap.IdentityTreeMap then tree else treeMap(tree)) match {
    case impl @ Template(constr, _, self, _) =>
      val tmap = withMappedLocalSyms(impl, self)
      val bodyCtx = ctx.withOwner(mapOwner(impl.symbol.owner))
      cpy.Template(impl)(
          constr = tmap.transformSub(constr),
          parents = impl.parents.mapconserve(transform),
          self = tmap.transformSub(self),
          body = impl.body.mapconserve:
            tmap.transform(_)(using bodyCtx)
        ).withType(tmap.mapType(impl.tpe))
    case tree1 =>
      val tree2 = if treeMapOnly then tree1 else withMappedType(tree1)
      tree2 match {
        case id: Ident =>
          if needsSelect(id.tpe) then
            try ref(id.tpe.asInstanceOf[TermRef]).withSpan(id.span)
            catch case ex: TypeError => super.transform(id)
          else
            super.transform(id)
        case sel: Select =>
          if needsIdent(sel.tpe) then
            ref(sel.tpe.asInstanceOf[TermRef]).withSpan(sel.span)
          else
            super.transform(sel)
        case app @ Apply(fun, args) =>
          val appCtx = sourceCtx(app)
          if app.tpe.isError then app
          else cpy.Apply(app)(transform(fun)(using appCtx), transform(args)(using appCtx))(using appCtx)
        case app @ TypeApply(fun, args) =>
          val appCtx = sourceCtx(app)
          if app.tpe.isError then app
          else cpy.TypeApply(app)(transform(fun)(using appCtx), transform(args)(using appCtx))(using appCtx)
        case typed @ Typed(expr, tpt) =>
          val typedCtx = sourceCtx(typed)
          if typed.tpe.isError then typed
          else cpy.Typed(typed)(transform(expr)(using typedCtx), transform(tpt)(using typedCtx))(using typedCtx)
        case nw @ New(tpt) =>
          val newCtx = sourceCtx(nw)
          if nw.tpe.isError then nw
          else cpy.New(nw)(transform(tpt)(using newCtx))(using newCtx)
        case named @ NamedArg(name, arg) =>
          val namedCtx = sourceCtx(named)
          if named.tpe.isError then named
          else cpy.NamedArg(named)(name, transform(arg)(using namedCtx))(using namedCtx)
        case assign @ Assign(lhs, rhs) =>
          val assignCtx = sourceCtx(assign)
          if assign.tpe.isError then assign
          else cpy.Assign(assign)(transform(lhs)(using assignCtx), transform(rhs)(using assignCtx))(using assignCtx)
        case blk @ Block(stats, expr) =>
          val (tmap1, stats1) = transformDefs(stats)
          val expr1 = tmap1.transform(expr)
          cpy.Block(blk)(stats1, expr1)
        case iff @ If(cond, thenp, elsep) =>
          val ifCtx = sourceCtx(iff)
          if iff.tpe.isError then iff
          else cpy.If(iff)(transform(cond)(using ifCtx), transform(thenp)(using ifCtx), transform(elsep)(using ifCtx))(using ifCtx)
        case closure @ Closure(env, meth, tpt) =>
          val closureCtx = sourceCtx(closure)
          if closure.tpe.isError then closure
          else cpy.Closure(closure)(transform(env)(using closureCtx), transform(meth)(using closureCtx), transform(tpt)(using closureCtx))(using closureCtx)
        case mtch @ Match(selector, cases) =>
          val matchCtx = sourceCtx(mtch)
          if mtch.tpe.isError then mtch
          else cpy.Match(mtch)(transform(selector)(using matchCtx), transformSub(cases)(using matchCtx))(using matchCtx)
        case loop @ WhileDo(cond, body) =>
          val loopCtx = sourceCtx(loop)
          if loop.tpe.isError then loop
          else cpy.WhileDo(loop)(transform(cond)(using loopCtx), transform(body)(using loopCtx))(using loopCtx)
        case tr @ Try(block, cases, finalizer) =>
          val tryCtx = sourceCtx(tr)
          if tr.tpe.isError then tr
          else cpy.Try(tr)(transform(block)(using tryCtx), transformSub(cases)(using tryCtx), transform(finalizer)(using tryCtx))(using tryCtx)
        case seq @ SeqLiteral(elems, elemtpt) =>
          val seqCtx = sourceCtx(seq)
          if seq.tpe.isError then seq
          else cpy.SeqLiteral(seq)(transform(elems)(using seqCtx), transform(elemtpt)(using seqCtx))(using seqCtx)
        case stt @ SingletonTypeTree(ref) =>
          val sttCtx = sourceCtx(stt)
          if stt.tpe.isError then stt
          else cpy.SingletonTypeTree(stt)(transform(ref)(using sttCtx))(using sttCtx)
        case rtt @ RefinedTypeTree(tpt, refinements) =>
          val rttCtx = sourceCtx(rtt)
          if rtt.tpe.isError then rtt
          else cpy.RefinedTypeTree(rtt)(transform(tpt)(using rttCtx), transformSub(refinements)(using rttCtx))(using rttCtx)
        case att @ AppliedTypeTree(tpt, args) =>
          val attCtx = sourceCtx(att)
          if att.tpe.isError then att
          else cpy.AppliedTypeTree(att)(transform(tpt)(using attCtx), transform(args)(using attCtx))(using attCtx)
        case bnt @ ByNameTypeTree(result) =>
          val bntCtx = sourceCtx(bnt)
          if bnt.tpe.isError then bnt
          else cpy.ByNameTypeTree(bnt)(transform(result)(using bntCtx))(using bntCtx)
        case tbt @ TypeBoundsTree(lo, hi, alias) =>
          val tbtCtx = sourceCtx(tbt)
          if tbt.tpe.isError then tbt
          else cpy.TypeBoundsTree(tbt)(transform(lo)(using tbtCtx), transform(hi)(using tbtCtx), transform(alias)(using tbtCtx))(using tbtCtx)
        case alt @ Alternative(trees) =>
          val altCtx = sourceCtx(alt)
          if alt.tpe.isError then alt
          else cpy.Alternative(alt)(transform(trees)(using altCtx))(using altCtx)
        case lit @ Literal(Constant(tpe: Type)) =>
          cpy.Literal(lit)(Constant(mapType(tpe)))
        case vdef: ValDef =>
          val res = super.transform(vdef).asInstanceOf[ValDef]
          noteTransformedMemberDef(res)
          res
        case ddef @ DefDef(name, paramss, tpt, _) =>
          val (tmap1, paramss1) = transformAllParamss(paramss)
          val res = cpy.DefDef(ddef)(name, paramss1, tmap1.transform(tpt), tmap1.transform(ddef.rhs))
          res.symbol.setParamssFromDefs(paramss1)
          res.symbol.transformAnnotations {
            case ann: BodyAnnotation => ann.derivedAnnotation(transform(ann.tree))
            case ann => ann
          }
          noteTransformedMemberDef(res)
          res
        case tdef: TypeDef =>
          val res = super.transform(tdef).asInstanceOf[TypeDef]
          noteTransformedMemberDef(res)
          res
        case tdef @ LambdaTypeTree(tparams, body) =>
          val (tmap1, tparams1) = transformDefs(tparams)
          cpy.LambdaTypeTree(tdef)(tparams1, tmap1.transform(body))
        case inlined: Inlined =>
          transformInlined(inlined)
        case cdef @ CaseDef(pat, guard, rhs) =>
          val tmap = withMappedSyms(patVars(pat))
          val pat1 = tmap.transform(pat)
          val guard1 = tmap.transform(guard)
          val rhs1 = tmap.transform(rhs)
          cpy.CaseDef(cdef)(pat1, guard1, rhs1)
        case labeled @ Labeled(bind, expr) =>
          val tmap = withMappedSym(bind.symbol)
          val bind1 = tmap.transformSub(bind)
          val expr1 = tmap.transform(expr)
          cpy.Labeled(labeled)(bind1, expr1)
        case tree1 =>
          super.transform(tree1)
      }
  }

  override def transformStats(trees: List[Tree], exprOwner: Symbol)(using Context): List[Tree] =
    transformDefs(trees)._2

  private def withMappedLocalSyms(stat1: Tree, stat2: Tree)(using Context): TreeTypeMap =
    val sym1 = if stat1.isDef && stat1.symbol.exists then stat1.symbol else NoSymbol
    val sym2 = if stat2.isDef && stat2.symbol.exists then stat2.symbol else NoSymbol
    if sym1 eq NoSymbol then
      if sym2 eq NoSymbol then this else withMappedSym(sym2)
    else if sym2 eq NoSymbol then withMappedSym(sym1)
    else withMappedSyms(sym1, sym2)

  def transformDefs[TT <: Tree](trees: List[TT])(using Context): (TreeTypeMap, List[TT]) = {
    var pending = trees
    var local1: Symbol = NoSymbol
    var local2: Symbol = NoSymbol
    var localBuf: mutable.ListBuffer[Symbol] | Null = null
    while pending.nonEmpty do
      val stat = pending.head
      if stat.isDef then
        val sym = stat.symbol
        if sym.exists then
          if local1 eq NoSymbol then local1 = sym
          else if local2 eq NoSymbol then local2 = sym
          else
            if localBuf == null then
              localBuf = new mutable.ListBuffer[Symbol]
              localBuf += local1
              localBuf += local2
            localBuf += sym
      pending = pending.tail

    if local1 eq NoSymbol then (this, transformSub(trees))
    else
      val tmap =
        if local2 eq NoSymbol then withMappedSym(local1)
        else if localBuf == null then withMappedSyms(local1, local2)
        else withMappedSyms(localBuf.toList)
      (tmap, tmap.transformSub(trees))
  }

  private def transformAllParamss(paramss: List[ParamClause]): (TreeTypeMap, List[ParamClause]) = paramss match
    case params :: paramss1 =>
      val (tmap1, params1: ParamClause) = ((params: @unchecked) match
        case ValDefs(vparams) => transformDefs(vparams)
        case TypeDefs(tparams) => transformDefs(tparams)
      ): @unchecked
      val (tmap2, paramss2) = tmap1.transformAllParamss(paramss1)
      (tmap2, params1 :: paramss2)
    case nil =>
      (this, paramss)

  def apply[ThisTree <: Tree](tree: ThisTree): ThisTree = transform(tree).asInstanceOf[ThisTree]

  def apply(annot: Annotation): Annotation = annot.derivedAnnotation(apply(annot.tree))

  private def sameSymbols(xs: List[Symbol], ys: List[Symbol]): Boolean =
    var xs1 = xs
    var ys1 = ys
    while xs1.nonEmpty && ys1.nonEmpty && (xs1.head eq ys1.head) do
      xs1 = xs1.tail
      ys1 = ys1.tail
    xs1.isEmpty && ys1.isEmpty

  /** The current tree map composed with a substitution [from -> to] */
  def withSubstitution(from: Symbol, to: Symbol): TreeTypeMap =
    if from eq to then this
    else {
      assert(!substToContains(from))
      assert(!substFromContains(to))
      assert(!newOwnerContains(from))
      assert(!oldOwnerContains(to))
      copy(
        typeMap,
        treeMap,
        from :: oldOwners,
        to :: newOwners,
        from :: substFrom,
        to :: substTo)
    }

  /** The current tree map composed with two substitutions [from1 -> to1, from2 -> to2] */
  def withSubstitution(from1: Symbol, from2: Symbol, to1: Symbol, to2: Symbol): TreeTypeMap =
    if (from1 eq to1) && (from2 eq to2) then this
    else {
      assert(!substToContains(from1) && !substToContains(from2))
      assert(!substFromContains(to1) && !substFromContains(to2))
      assert(!newOwnerContains(from1) && !newOwnerContains(from2))
      assert(!oldOwnerContains(to1) && !oldOwnerContains(to2))
      copy(
        typeMap,
        treeMap,
        from1 :: from2 :: oldOwners,
        to1 :: to2 :: newOwners,
        from1 :: from2 :: substFrom,
        to1 :: to2 :: substTo)
    }

  /** The current tree map composed with a substitution [from -> to] */
  def withSubstitution(from: List[Symbol], to: List[Symbol]): TreeTypeMap =
    if (from eq to) || sameSymbols(from, to) then this
    else {
      // assert that substitution stays idempotent, assuming its parts are
      // TODO: It might be better to cater for the asserted-away conditions, by
      // setting up a proper substitution abstraction with a compose operator that
      // guarantees idempotence. But this might be too inefficient in some cases.
      // We'll cross that bridge when we need to.
      assert(!hasAnyInSubstTo(from))
      assert(!hasAnyInSubstFrom(to))
      assert(!hasAnyInNewOwners(from))
      assert(!hasAnyInOldOwners(to))
      copy(
        typeMap,
        treeMap,
        from ++ oldOwners,
        to ++ newOwners,
        from ++ substFrom,
        to ++ substTo)
    }

  /** Apply `typeMap` and `ownerMap` to given symbols `syms`
   *  and return a treemap that contains the substitution
   *  between original and mapped symbols.
   */
  def withMappedSym(sym: Symbol): TreeTypeMap =
    if sym.isClass then
      val syms = sym :: Nil
      withMappedSymsGeneral(syms, mapSymbols(syms, this))
    else withMappedSym(sym, mapSymbol(sym, this))

  /** Apply `typeMap` and `ownerMap` to two symbols and return a treemap
   *  that contains the substitution between originals and mapped symbols.
   */
  def withMappedSyms(sym1: Symbol, sym2: Symbol): TreeTypeMap =
    if sym1.isClass || sym2.isClass || (sym1 eq sym2) then
      val syms = sym1 :: sym2 :: Nil
      withMappedSymsGeneral(syms, mapSymbols(syms, this))
    else
      val (mapped1, mapped2) = mapTwoSymbols(sym1, sym2, this)
      withMappedSyms(sym1, sym2, mapped1, mapped2)

  /** Apply `typeMap` and `ownerMap` to given symbols `syms`
   *  and return a treemap that contains the substitution
   *  between original and mapped symbols.
   */
  def withMappedSyms(syms: List[Symbol]): TreeTypeMap = syms match
    case Nil => this
    case sym :: Nil => withMappedSym(sym)
    case sym1 :: sym2 :: Nil => withMappedSyms(sym1, sym2)
    case _ => withMappedSyms(syms, mapSymbols(syms, this))

  protected def withMappedSym(sym: Symbol, mapped: Symbol): TreeTypeMap =
    if (sym eq mapped) then this
    else if sym.isClass || mapped.isClass then withMappedSymsGeneral(sym :: Nil, mapped :: Nil)
    else withSubstitution(sym, mapped)

  protected def withMappedSyms(sym1: Symbol, sym2: Symbol, mapped1: Symbol, mapped2: Symbol): TreeTypeMap =
    if (sym1 eq mapped1) && (sym2 eq mapped2) then this
    else if sym1.isClass || sym2.isClass || mapped1.isClass || mapped2.isClass || (sym1 eq sym2) then
      withMappedSymsGeneral(sym1 :: sym2 :: Nil, mapped1 :: mapped2 :: Nil)
    else withSubstitution(sym1, sym2, mapped1, mapped2)

  /** The tree map with the substitution between originals `syms`
   *  and mapped symbols `mapped`. Also goes into mapped classes
   *  and substitutes their declarations.
   */
  def withMappedSyms(syms: List[Symbol], mapped: List[Symbol]): TreeTypeMap = (syms, mapped) match
    case (Nil, Nil) => this
    case (sym :: Nil, mappedSym :: Nil) => withMappedSym(sym, mappedSym)
    case (sym1 :: sym2 :: Nil, mapped1 :: mapped2 :: Nil) =>
      withMappedSyms(sym1, sym2, mapped1, mapped2)
    case _ =>
      withMappedSymsGeneral(syms, mapped)

  private def withMappedSymsGeneral(syms: List[Symbol], mapped: List[Symbol]): TreeTypeMap =
    if (syms eq mapped) || sameSymbols(syms, mapped) then this
    else
      val substMap = withSubstitution(syms, mapped)
      lazy val origCls = mapped.zip(syms).filter(_._1.isClass).toMap
      mapped.filter(_.isClass).foldLeft(substMap) { (tmap, cls) =>
        val origDcls = cls.info.decls.toList.filterNot(_.is(TypeParam))
        val tmap0 = tmap.withSubstitution(origCls(cls).typeParams, cls.typeParams)
        val mappedDcls = mapSymbols(origDcls, tmap0, mapAlways = true)
        val tmap1 = tmap.withMappedSyms(
          origCls(cls).typeParams ::: origDcls,
          cls.typeParams ::: mappedDcls)
        mapped.foreach { sym =>
          // outer Symbols can reference nested ones in info,
          // so we remap that once again with the updated TreeTypeMap
          sym.info = tmap1.mapType(sym.info)
        }
        origDcls.lazyZip(mappedDcls).foreach(cls.asClass.replace)
        tmap1
      }

  override def toString =
    def showSyms(syms: List[Symbol]) =
      syms.map(sym => s"$sym#${sym.id}").mkString(", ")
    s"""TreeTypeMap(
       |typeMap   = $typeMap
       |treeMap   = $treeMap
       |oldOwners = ${showSyms(oldOwners)}
       |newOwners = ${showSyms(newOwners)}
       |substFrom = ${showSyms(substFrom)}
       |substTo   = ${showSyms(substTo)}""".stripMargin
}
