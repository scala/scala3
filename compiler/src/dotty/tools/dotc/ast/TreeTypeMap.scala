package dotty.tools
package dotc
package ast

import core.*
import Types.*, Contexts.*, Flags.*
import Symbols.*, Annotations.*, Trees.*, Symbols.*, Constants.Constant
import Decorators.*
import config.Config


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
  val treeMap: tpd.Tree => tpd.Tree = identity,
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

  // `mapOwnerThis` only substitutes ThisType prefixes whose `cls` matches a
  // ClassSymbol in `oldOwners`; entries that aren't ClassSymbols are stepped
  // over without effect. When `oldOwners` contains no ClassSymbols at all
  // (e.g. Inliner's initial `inlinedMethod :: Nil`), the whole TypeMap walk
  // is a no-op on every NamedType visited — precompute the gate once.
  private val hasOwnerClass: Boolean =
    var xs = oldOwners
    var found = false
    while !found && (xs ne Nil) do
      if xs.head.isClass then found = true
      xs = xs.tail
    found

  /** If `sym` is one of `oldOwners`, replace by corresponding symbol in `newOwners` */
  def mapOwner(sym: Symbol): Symbol = sym.subst(oldOwners, newOwners)

  /** Replace occurrences of `This(oldOwner)` in some prefix of a type
   *  by the corresponding `This(newOwner)`.
   */
  private val mapOwnerThis = new TypeMap {
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

  // Cached so the SubstSymMap's IdentityHashMap lookup (built once) is reused
  // across all mapType calls on this TreeTypeMap, instead of re-walking substFrom
  // for every NamedType.
  private lazy val cachedSubstSymMap: Substituters.SubstSymMap =
    new Substituters.SubstSymMap(substFrom, substTo)

  private lazy val substMap: TypeMap = new TypeMap():
    def apply(tp: Type): Type = tp match
      case tp: TermRef if tp.symbol.isImport => mapOver(tp)
      case tp => cachedSubstSymMap(tp)

  // Identity-keyed cache for `mapType`: Types are uniqued, so the same Type
  // input always produces the same mapped output for a given TreeTypeMap.
  // The same `tree.tpe` is re-asked many times when an inliner's TreeTypeMap
  // walks a tree. Lazily allocated on first non-trivial mapType call so
  // trivial maps (no substFrom + no owner remap) pay no overhead.
  private var myMapTypeCache: util.EqHashMap[Type, Type] | Null = null
  private val shouldCacheMapType = cacheTypeMap || substFrom.nonEmpty || oldOwners.nonEmpty

  def mapType(tp: Type): Type =
    // Cache only when we actually do non-trivial work (substSym walk and/or
    // ownerThis walk), or when a deterministic custom typeMap explicitly opts in.
    // Pure `typeMap(tp)` calls stay uncached by default because they may depend on
    // mutable state or have their own caching.
    if shouldCacheMapType then
      var cache = myMapTypeCache
      if cache == null then
        cache = util.EqHashMap[Type, Type]()
        myMapTypeCache = cache
      val hit = cache.lookup(tp)
      if hit != null then return hit
      val res = computeMapType(tp)
      cache.update(tp, res)
      res
    else computeMapType(tp)
  end mapType

  private def computeMapType(tp: Type): Type =
    val tp1 = typeMap(tp)
    val tp2 = if substFrom.isEmpty then tp1 else substMap(tp1)
    // Fast path: when no ClassSymbol appears in `oldOwners`, `mapOwnerThis`
    // is the identity (the recursion in `mapPrefix` only substitutes for
    // ClassSymbol entries). Skip the TypeMap walk in that case.
    if !hasOwnerClass then tp2 else mapOwnerThis(tp2)
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
    val tpe1 = mapType(tpe)
    if (tpe1 eq tpe) && !tpe1.isInstanceOf[ErrorType] && !Config.checkTreesConsistent then tree
    else tree.withType(tpe1)

  private def sourceCtx(tree: Tree)(using Context): Context =
    val source = tree.source
    if source.exists && (source ne ctx.source) then ctx.withSource(source) else ctx

  override def transform(tree: Tree)(using Context): Tree = treeMap(tree) match {
    case impl @ Template(constr, _, self, _) =>
      val tmap = withMappedSyms(localSyms(impl :: self :: Nil))
      val bodyCtx = ctx.withOwner(mapOwner(impl.symbol.owner))
      cpy.Template(impl)(
          constr = tmap.transformSub(constr),
          parents = impl.parents.mapconserve(transform),
          self = tmap.transformSub(self),
          body = impl.body.mapconserve:
            tmap.transform(_)(using bodyCtx)
        ).withType(tmap.mapType(impl.tpe))
    case tree1 =>
      withMappedType(tree1) match {
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
        case ddef @ DefDef(name, paramss, tpt, _) =>
          val (tmap1, paramss1) = transformAllParamss(paramss)
          val res = cpy.DefDef(ddef)(name, paramss1, tmap1.transform(tpt), tmap1.transform(ddef.rhs))
          res.symbol.setParamssFromDefs(paramss1)
          res.symbol.transformAnnotations {
            case ann: BodyAnnotation => ann.derivedAnnotation(transform(ann.tree))
            case ann => ann
          }
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
          val tmap = withMappedSyms(bind.symbol :: Nil)
          val bind1 = tmap.transformSub(bind)
          val expr1 = tmap.transform(expr)
          cpy.Labeled(labeled)(bind1, expr1)
        case tree1 =>
          super.transform(tree1)
      }
  }

  override def transformStats(trees: List[Tree], exprOwner: Symbol)(using Context): List[Tree] =
    transformDefs(trees)._2

  def transformDefs[TT <: Tree](trees: List[TT])(using Context): (TreeTypeMap, List[TT]) = {
    val tmap = withMappedSyms(localSyms(trees))
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

  /** The current tree map composed with a substitution [from -> to] */
  def withSubstitution(from: List[Symbol], to: List[Symbol]): TreeTypeMap =
    if (from eq to) this
    else {
      // assert that substitution stays idempotent, assuming its parts are
      // TODO: It might be better to cater for the asserted-away conditions, by
      // setting up a proper substitution abstraction with a compose operator that
      // guarantees idempotence. But this might be too inefficient in some cases.
      // We'll cross that bridge when we need to.
      assert(!from.exists(substTo contains _))
      assert(!to.exists(substFrom contains _))
      assert(!from.exists(newOwners contains _))
      assert(!to.exists(oldOwners contains _))
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
  def withMappedSyms(syms: List[Symbol]): TreeTypeMap =
    withMappedSyms(syms, mapSymbols(syms, this))

  /** The tree map with the substitution between originals `syms`
   *  and mapped symbols `mapped`. Also goes into mapped classes
   *  and substitutes their declarations.
   */
  def withMappedSyms(syms: List[Symbol], mapped: List[Symbol]): TreeTypeMap =
    if syms eq mapped then this
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
