package dotty.tools
package dotc
package ast

import core.*
import Types.*, Contexts.*, Flags.*
import Symbols.*, Annotations.*, Trees.*, Symbols.*, Constants.Constant
import Decorators.*


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
  val oldOwners: Vector[Symbol] = Vector(),
  val newOwners: Vector[Symbol] = Vector(),
  val substFrom: Vector[Symbol] = Vector(),
  val substTo: Vector[Symbol] = Vector(),
  cpy: tpd.TreeCopier = tpd.cpy)(using Context) extends tpd.TreeMap(cpy) {
  import tpd.*

  private inline val MaxVector1Length = 32

  def copy(
      typeMap: Type => Type,
      treeMap: Tree => Tree,
      oldOwners: Vector[Symbol],
      newOwners: Vector[Symbol],
      substFrom: Vector[Symbol],
      substTo: Vector[Symbol])(using Context): TreeTypeMap =
    new TreeTypeMap(typeMap, treeMap, oldOwners, newOwners, substFrom, substTo)

  private def concatSymbols(left: Vector[Symbol], right: Vector[Symbol]): Vector[Symbol] =
    if left.isEmpty then right
    else if right.isEmpty then left
    else
      val leftLen = left.length
      val rightLen = right.length
      val len = leftLen + rightLen
      if len <= MaxVector1Length then
        val elems = new Array[AnyRef](len)
        var i = 0
        while i < leftLen do
          elems(i) = left(i).asInstanceOf[AnyRef]
          i += 1
        var j = 0
        while j < rightLen do
          elems(leftLen + j) = right(j).asInstanceOf[AnyRef]
          j += 1
        Vector.fromArray1Unsafe(elems).asInstanceOf[Vector[Symbol]]
      else
        val b = Vector.newBuilder[Symbol]
        b.sizeHint(len)
        var i = 0
        while i < leftLen do
          b += left(i)
          i += 1
        var j = 0
        while j < rightLen do
          b += right(j)
          j += 1
        b.result()

  /** If `sym` is one of `oldOwners`, replace by corresponding symbol in `newOwners` */
  def mapOwner(sym: Symbol): Symbol = sym.subst(oldOwners, newOwners)

  /** Replace occurrences of `This(oldOwner)` in some prefix of a type
   *  by the corresponding `This(newOwner)`.
   */
  assert(oldOwners.length == newOwners.length, i"mismatched owners: $oldOwners --> $newOwners")
  assert(substFrom.length == substTo.length, i"mismatched substitution: $substFrom --> $substTo")

  private val classOwnerCount =
    var count = 0
    var i = 0
    while i < oldOwners.length do
      if oldOwners(i).isClass then count += 1
      i += 1
    count

  private val oldOwnerClasses = new Array[ClassSymbol](classOwnerCount)
  private val newOwnerThisTypes = new Array[Type](classOwnerCount)

  private def initOwnerThisSubst(): Unit =
    var i = 0
    var j = 0
    while i < oldOwners.length do
      oldOwners(i) match
        case cls: ClassSymbol =>
          oldOwnerClasses(j) = cls
          newOwnerThisTypes(j) = newOwners(i).thisType
          j += 1
        case _ =>
      i += 1

  initOwnerThisSubst()

  private val mapOwnerThis = new TypeMap {
    private def mapPrefix(tp: Type): Type =
      var result = tp
      var i = 0
      while i < oldOwnerClasses.length do
        result = result.substThis(oldOwnerClasses(i), newOwnerThisTypes(i))
        i += 1
      result
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => tp.derivedSelect(mapPrefix(tp.prefix))
      case _ => mapOver(tp)
    }
  }

  private val substFrom1 = if substFrom.length >= 1 then substFrom(0) else NoSymbol
  private val substTo1 = if substTo.length >= 1 then substTo(0) else NoSymbol
  private val substFrom2 = if substFrom.length >= 2 then substFrom(1) else NoSymbol
  private val substTo2 = if substTo.length >= 2 then substTo(1) else NoSymbol
  private val symbolSubstData: Substituters.SubstSymData | Null =
    substFrom.length match
      case 0 => null
      case 1 | 2 => null
      case _ => Substituters.SubstSymData(substFrom, substTo)

  private def substSymbols(tp: Type)(using Context): Type =
    substFrom.length match
      case 0 => tp
      case 1 =>
        val substMap = new TypeMap():
          def apply(tp: Type): Type = tp match
            case tp: TermRef if tp.symbol.isImport => mapOver(tp)
            case tp => Substituters.substSym1(tp, substFrom1, substTo1, null)
        substMap(tp)
      case 2 =>
        val substMap = new TypeMap():
          def apply(tp: Type): Type = tp match
            case tp: TermRef if tp.symbol.isImport => mapOver(tp)
            case tp => Substituters.substSym2(tp, substFrom1, substTo1, substFrom2, substTo2, null)
        substMap(tp)
      case _ =>
        symbolSubstData.nn.substImportAware(tp)

  def mapType(tp: Type): Type =
    val tp1 = typeMap(tp)
    val tp2 = substSymbols(tp1)
    if oldOwnerClasses.length == 0 then tp2 else mapOwnerThis(tp2)

  private def containsSymbol(syms: Vector[Symbol], sym: Symbol): Boolean =
    var i = 0
    while i < syms.length do
      if syms(i) == sym then return true
      i += 1
    false

  private def containsAny(syms: Vector[Symbol], targets: Vector[Symbol]): Boolean =
    var i = 0
    while i < syms.length do
      if containsSymbol(targets, syms(i)) then return true
      i += 1
    false

  private def updateDecls(prevStats: Vector[Tree], newStats: Vector[Tree]): Unit =
    assert(prevStats.length == newStats.length)
    var i = 0
    while i < prevStats.length do
      prevStats(i) match
        case pdef: MemberDef =>
          val prevSym = pdef.symbol
          val newSym = newStats(i).symbol
          val newCls = newSym.owner.asClass
          if (prevSym != newSym) newCls.replace(prevSym, newSym)
        case _ =>
      i += 1

  def transformInlined(tree: Inlined)(using Context): Tree =
    val Inlined(call, bindings, expanded) = tree
    val (tmap1, bindings1) = transformDefs(bindings)
    val expanded1 = tmap1.transform(expanded)
    cpy.Inlined(tree)(call, bindings1, expanded1)

  override def transform(tree: Tree)(using Context): Tree = treeMap(tree) match {
    case impl @ Template(constr, _, self, _) =>
      val tmap = withMappedSyms(localSyms(Vector(impl, self)))
      cpy.Template(impl)(
          constr = tmap.transformSub(constr),
          parents = impl.parents.mapconserve(transform),
          self = tmap.transformSub(self),
          body = impl.body.mapconserve:
            tmap.transform(_)(using ctx.withOwner(mapOwner(impl.symbol.owner)))
        ).withType(tmap.mapType(impl.tpe))
    case tree1 =>
      tree1.withType(mapType(tree1.tpe)) match {
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
        case app: Apply =>
          super.transform(app)
        case blk @ Block(stats, expr) =>
          val (tmap1, stats1) = transformDefs(stats)
          val expr1 = tmap1.transform(expr)
          cpy.Block(blk)(stats1, expr1)
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
          val tmap = withMappedSyms(Vector(bind.symbol))
          val bind1 = tmap.transformSub(bind)
          val expr1 = tmap.transform(expr)
          cpy.Labeled(labeled)(bind1, expr1)
        case tree1 =>
          super.transform(tree1)
      }
  }

  override def transformStats(trees: Vector[Tree], exprOwner: Symbol)(using Context): Vector[Tree] =
    transformDefs(trees)._2

  def transformDefs[TT <: Tree](trees: Vector[TT])(using Context): (TreeTypeMap, Vector[TT]) = {
    val tmap = withMappedSyms(localSyms(trees))
    (tmap, tmap.transformSub(trees))
  }

  private def transformAllParamss(paramss: Vector[ParamClause]): (TreeTypeMap, Vector[ParamClause]) = paramss match
    case params +: paramss1 =>
      val (tmap1, params1: ParamClause) = ((params: @unchecked) match
        case ValDefs(vparams) => transformDefs(vparams)
        case TypeDefs(tparams) => transformDefs(tparams)
      ): @unchecked
      val (tmap2, paramss2) = tmap1.transformAllParamss(paramss1)
      (tmap2, params1 +: paramss2)
    case nil =>
      (this, paramss)

  def apply[ThisTree <: Tree](tree: ThisTree): ThisTree = transform(tree).asInstanceOf[ThisTree]

  def apply(annot: Annotation): Annotation = annot.derivedAnnotation(apply(annot.tree))

  /** The current tree map composed with a substitution [from -> to] */
  def withSubstitution(from: Vector[Symbol], to: Vector[Symbol]): TreeTypeMap =
    if (from eq to) this
    else {
      // assert that substitution stays idempotent, assuming its parts are
      // TODO: It might be better to cater for the asserted-away conditions, by
      // setting up a proper substitution abstraction with a compose operator that
      // guarantees idempotence. But this might be too inefficient in some cases.
      // We'll cross that bridge when we need to.
      assert(!containsAny(from, substTo))
      assert(!containsAny(to, substFrom))
      assert(!containsAny(from, newOwners))
      assert(!containsAny(to, oldOwners))
      copy(
        typeMap,
        treeMap,
        concatSymbols(from, oldOwners),
        concatSymbols(to, newOwners),
        concatSymbols(from, substFrom),
        concatSymbols(to, substTo))
    }

  /** Apply `typeMap` and `ownerMap` to given symbols `syms`
   *  and return a treemap that contains the substitution
   *  between original and mapped symbols.
   */
  def withMappedSyms(syms: Vector[Symbol]): TreeTypeMap =
    withMappedSyms(syms, mapSymbols(syms, this))

  /** The tree map with the substitution between originals `syms`
   *  and mapped symbols `mapped`. Also goes into mapped classes
   *  and substitutes their declarations.
   */
  def withMappedSyms(syms: Vector[Symbol], mapped: Vector[Symbol]): TreeTypeMap =
    if syms eq mapped then this
    else
      var tmap = withSubstitution(syms, mapped)
      var clsIdx = 0
      while clsIdx < mapped.length do
        val cls = mapped(clsIdx)
        if cls.isClass then {
          val origCls = syms(clsIdx)
          val origDcls = cls.info.decls.toVector.filterNot(_.is(TypeParam))
          val tmap0 = tmap.withSubstitution(origCls.typeParams, cls.typeParams)
          val mappedDcls = mapSymbols(origDcls, tmap0, mapAlways = true)
          val tmap1 = tmap.withMappedSyms(
            concatSymbols(origCls.typeParams, origDcls),
            concatSymbols(cls.typeParams, mappedDcls))
          var mappedIdx = 0
          while mappedIdx < mapped.length do
            val sym = mapped(mappedIdx)
            // outer Symbols can reference nested ones in info,
            // so we remap that once again with the updated TreeTypeMap
            sym.info = tmap1.mapType(sym.info)
            mappedIdx += 1
          val cls1 = cls.asClass
          var dclIdx = 0
          while dclIdx < origDcls.length do
            cls1.replace(origDcls(dclIdx), mappedDcls(dclIdx))
            dclIdx += 1
          tmap = tmap1
        }
        clsIdx += 1
      tmap

  override def toString =
    def showSyms(syms: Vector[Symbol]) =
      syms.map(sym => s"$sym#${sym.id}").mkString(", ")
    s"""TreeTypeMap(
       |typeMap   = $typeMap
       |treeMap   = $treeMap
       |oldOwners = ${showSyms(oldOwners)}
       |newOwners = ${showSyms(newOwners)}
       |substFrom = ${showSyms(substFrom)}
       |substTo   = ${showSyms(substTo)}""".stripMargin
}
