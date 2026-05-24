package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, Decorators.*, TypeApplications.*
import util.{SimpleIdentitySet, SimpleIdentityMap}
import collection.mutable
import printing.Printer
import printing.Texts.*
import config.Config
import config.Printers.constr
import reflect.ClassTag
import annotation.tailrec
import annotation.internal.sharable
import cc.{CapturingType, derivedCapturingType}

import scala.compiletime.uninitialized

object OrderingConstraint {

  /** If true, use reverse dependencies in `replace` to avoid checking the bounds
   *  of all parameters in the constraint. This can speed things up, but there are some
   *  rare corner cases where reverse dependencies miss a parameter. Specifically,
   *  if a constraint contains a free reference to TypeParam P and afterwards the
   *  same P is added as a bound variable to the constraint, a backwards link would
   *  then become necessary at this point but is missing. This causes two CB projects
   *  to fail when reverse dependencies are checked (parboiled2 and perspective).
   *  In these rare cases `replace` could behave differently when optimized. However,
   *  no deviation was found in the two projects. It is not clear what the "right"
   *  behavior of `replace` should be in these cases. Normally, PolyTypes added
   *  to constraints are supposed to be fresh, so that would mean that the behavior
   *  with optimizeReplace = true would be correct. But the previous behavior without
   *  reverse dependency checking corresponds to `optimizeReplace = false`. This behavior
   *  makes sense if we assume that the added polytype was simply added too late, so we
   *  want to establish the link between newly bound variable and pre-existing reference.
   */
  private final val optimizeReplace = true

  private type ArrayValuedMap[T] = SimpleIdentityMap[TypeLambda, Array[T]]

  /** The type of `OrderingConstraint#boundsMap` */
  private type ParamBounds = ArrayValuedMap[Type]

  /** The type of `OrderingConstraint#lowerMap`, `OrderingConstraint#upperMap` */
  private type ParamOrdering = ArrayValuedMap[List[TypeParamRef]]

  /** A lens for updating a single entry array in one of the three constraint maps */
  private abstract class ConstraintLens[T <: AnyRef: ClassTag] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[T] | Null
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[T])(using Context): OrderingConstraint
    def initial: T

    def apply(c: OrderingConstraint, poly: TypeLambda, idx: Int): T = {
      val es = entries(c, poly)
      if (es == null) initial else es(idx)
    }

    /** The `current` constraint but with the entry for `param` updated to `entry`.
     *  `current` is used linearly. If it is different from `prev` then `current` is
     *  known to be dead after the call. Hence it is OK to update destructively
     *  parts of `current` which are not shared by `prev`.
     */
    def update(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, entry: T)(using Context): OrderingConstraint = {
      var es = entries(current, poly)
      // TODO: investigate why flow typing is not working on `es`
      if (es != null && (es.nn(idx) eq entry)) current
      else {
        val result =
          if (es == null) {
            es = Array.fill(poly.paramNames.length)(initial)
            updateEntries(current, poly, es.nn)
          }
          else {
            val prev_es = entries(prev, poly)
            if (prev_es == null || (es.nn ne prev_es))
              current // can re-use existing entries array.
            else {
              es = es.nn.clone
              updateEntries(current, poly, es.nn)
            }
          }
        es.nn(idx) = entry
        result
      }
    }

    def update(prev: OrderingConstraint, current: OrderingConstraint,
        param: TypeParamRef, entry: T)(using Context): OrderingConstraint =
      update(prev, current, param.binder, param.paramNum, entry)

    def map(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, f: T => T)(using Context): OrderingConstraint =
     update(prev, current, poly, idx, f(apply(current, poly, idx)))

    def map(prev: OrderingConstraint, current: OrderingConstraint,
        param: TypeParamRef, f: T => T)(using Context): OrderingConstraint =
      map(prev, current, param.binder, param.paramNum, f)
  }

  private val boundsLens: ConstraintLens[Type] = new ConstraintLens[Type] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[Type] | Null =
      c.boundsMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[Type])(using Context): OrderingConstraint =
      c.newConstraint(boundsMap = c.boundsMap.updated(poly, entries))
    def initial = NoType
  }

  private val lowerLens: ConstraintLens[List[TypeParamRef]] = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] | Null =
      c.lowerMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(using Context): OrderingConstraint =
      c.newConstraint(lowerMap = c.lowerMap.updated(poly, entries))
    def initial = Nil
  }

  private val upperLens: ConstraintLens[List[TypeParamRef]] = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] | Null =
      c.upperMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(using Context): OrderingConstraint =
      c.newConstraint(upperMap = c.upperMap.updated(poly, entries))
    def initial = Nil
  }

  @sharable
  val empty = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentitySet.empty)
}

import OrderingConstraint.*

/** Constraint over undetermined type parameters that keeps separate maps to
 *  reflect parameter orderings.
 *  @param boundsMap a map from TypeLambda to arrays.
 *               Each array contains twice the number of entries as there a type parameters
 *               in the TypeLambda. The first half of the array contains the type bounds that constrain the
 *               lambda's type parameters. The second half might contain type variables that
 *               track the corresponding parameters, or is left empty (filled with nulls).
 *               An instantiated type parameter is represented by having its instance type in
 *               the corresponding array entry. The dual use of arrays for poly params
 *               and typevars is to save space and hopefully gain some speed.
 *
 *  @param lowerMap a map from TypeLambdas to arrays. Each array entry corresponds
 *               to a parameter P of the type lambda; it contains all constrained parameters
 *               Q that are known to be smaller than P, i.e. Q <: P.
 *  @param upperMap a map from TypeLambdas to arrays. Each array entry corresponds
 *               to a parameter P of the type lambda; it contains all constrained parameters
 *               Q that are known to be greater than P, i.e. P <: Q.
 *  @param hardVars a set of type variables that are marked as hard and therefore will not
 *               undergo a `widenUnion` when instantiated to their lower bound.
 */
class OrderingConstraint(private val boundsMap: ParamBounds,
                         private val lowerMap : ParamOrdering,
                         private val upperMap : ParamOrdering,
                         private val hardVars : TypeVars) extends Constraint {
  thisConstraint =>

  import UnificationDirection.*

  type This = OrderingConstraint

  /** A new constraint with given maps and given set of hard typevars */
  private def newConstraint(
    boundsMap: ParamBounds = this.boundsMap,
    lowerMap: ParamOrdering = this.lowerMap,
    upperMap: ParamOrdering = this.upperMap,
    hardVars: TypeVars = this.hardVars)(using Context) : OrderingConstraint =
      if boundsMap.isEmpty && lowerMap.isEmpty && upperMap.isEmpty then
        empty
      else
        val result = new OrderingConstraint(boundsMap, lowerMap, upperMap, hardVars)
        if ctx.run != null then ctx.run.nn.recordConstraintSize(result, result.boundsMap.size)
        result.coDeps = this.coDeps
        result.contraDeps = this.contraDeps
        result.dirtyDeps = this.dirtyDeps
        result

// ----------- Basic indices --------------------------------------------------

  /** The number of type parameters in the given entry array */
  private def paramCount(entries: Array[Type]) = entries.length >> 1

  /** The type variable corresponding to parameter numbered `n`, null if none was created */
  private def typeVar(entries: Array[Type], n: Int): Type | Null =
    entries(paramCount(entries) + n)

  // 1-slot last-binder caches for map lookups: consecutive queries during
  // a single isSubType/compareTypeParamRef typically hit the same binder, so
  // skipping the megamorphic SimpleIdentityMap walk is a measurable win.
  // This OrderingConstraint instance is replaced (not mutated) on map
  // structural updates, so the cache is implicitly per-snapshot. The
  // ConstraintLens.update path may mutate an entries array in place when the
  // current constraint is the unique live reference; in that case any cached
  // pointer still references the same (now updated) array, which is what we want.
  // Cache fields are private to this instance.
  private var lastEntryBinder: TypeLambda | Null = null
  private var lastEntryEntries: Array[Type] | Null = null
  private var lastLowerBinder: TypeLambda | Null = null
  private var lastLowerEntries: Array[List[TypeParamRef]] | Null = null
  private var lastUpperBinder: TypeLambda | Null = null
  private var lastUpperEntries: Array[List[TypeParamRef]] | Null = null

  /** The `boundsMap` entries corresponding to `binder` */
  private inline def entries(binder: TypeLambda): Array[Type] | Null =
    if binder eq lastEntryBinder then lastEntryEntries
    else
      val es = boundsMap(binder)
      lastEntryBinder = binder
      lastEntryEntries = es
      es

  /** The `lowerMap` entries corresponding to `binder` */
  private inline def lowerEntries(binder: TypeLambda): Array[List[TypeParamRef]] | Null =
    if binder eq lastLowerBinder then lastLowerEntries
    else
      val es = lowerMap(binder)
      lastLowerBinder = binder
      lastLowerEntries = es
      es

  /** The `upperMap` entries corresponding to `binder` */
  private inline def upperEntries(binder: TypeLambda): Array[List[TypeParamRef]] | Null =
    if binder eq lastUpperBinder then lastUpperEntries
    else
      val es = upperMap(binder)
      lastUpperBinder = binder
      lastUpperEntries = es
      es

  /** The `boundsMap` entry corresponding to `param` */
  def entry(param: TypeParamRef): Type = {
    val entries = this.entries(param.binder)
    if (entries == null) NoType
    else entries(param.paramNum)
  }

// ----------- Contains tests --------------------------------------------------

  def contains(pt: TypeLambda): Boolean = boundsMap(pt) != null

  def contains(param: TypeParamRef): Boolean = {
    val entries = this.entries(param.binder)
    entries != null && isBounds(entries(param.paramNum))
  }

  def contains(tvar: TypeVar): Boolean = {
    val origin = tvar.origin
    val entries = this.entries(origin.binder)
    val pnum = origin.paramNum
    entries != null && isBounds(entries(pnum)) && (typeVar(entries, pnum) eq tvar)
  }

// ---------- Dependency handling ----------------------------------------------

  def lower(param: TypeParamRef): List[TypeParamRef] =
    val entries = lowerEntries(param.binder)
    if entries == null then Nil else entries(param.paramNum)

  def upper(param: TypeParamRef): List[TypeParamRef] =
    val entries = upperEntries(param.binder)
    if entries == null then Nil else entries(param.paramNum)

  def minLower(param: TypeParamRef): List[TypeParamRef] = {
    val all = lower(param)
    all.filterNot(p => all.exists(isLess(p, _)))
  }

  def minUpper(param: TypeParamRef): List[TypeParamRef] = {
    val all = upper(param)
    all.filterNot(p => all.exists(isLess(_, p)))
  }

  def exclusiveLower(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef] =
    lower(param).filterNot(isLess(_, butNot))

  def exclusiveUpper(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef] =
    upper(param).filterNot(isLess(butNot, _))

  def bounds(param: TypeParamRef)(using Context): TypeBounds = {
    val e = entry(param)
    if (e.exists) e.bounds
    else param.binder.paramInfos(param.paramNum)
  }

// ---------- Info related to TypeParamRefs -------------------------------------------

  def isLess(param1: TypeParamRef, param2: TypeParamRef): Boolean =
    upper(param1).contains(param2)

  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    entry(param).bounds

  def typeVarOfParam(param: TypeParamRef): Type =
    val entries = this.entries(param.binder)
    if entries == null then NoType
    else
      val tvar = typeVar(entries, param.paramNum)
      if tvar == null then NoType
      else tvar

// ------------- Type parameter dependencies ----------------------------------------

  private type ReverseDeps = SimpleIdentityMap[TypeParamRef, SimpleIdentitySet[TypeParamRef]]

  /** A map that associates type parameters of this constraint with all other type
   *  parameters that refer to them in their bounds covariantly, such that, if the
   *  type parameter is instantiated to a larger type, the constraint would be narrowed
   *  (i.e. solution set changes other than simply being made larger).
   */
  private var coDeps: ReverseDeps = SimpleIdentityMap.empty

  /** A map that associates type parameters of this constraint with all other type
   *  parameters that refer to them in their bounds contravariantly, such that, if the
   *  type parameter is instantiated to a smaller type, the constraint would be narrowed.
   *  (i.e. solution set changes other than simply being made smaller).
   */
  private var contraDeps: ReverseDeps = SimpleIdentityMap.empty

  /** Type lambdas whose bounds have not yet been indexed into `coDeps` and
   *  `contraDeps`. Bounds are added speculatively by `init`, while most
   *  constraints are solved or discarded without ever querying reverse deps.
   */
  private var dirtyDeps: SimpleIdentitySet[TypeLambda] = SimpleIdentitySet.empty

  /** Null-safe indexing */
  extension (deps: ReverseDeps) def at(param: TypeParamRef): SimpleIdentitySet[TypeParamRef] =
    val result = deps(param)
    if null == result  // swapped operand order important since `==` is overloaded in `SimpleIdentitySet`
    then SimpleIdentitySet.empty
    else result

  /** Mark dependencies for `poly` to be indexed on demand. */
  private def markDepsDirty(poly: TypeLambda): this.type =
    dirtyDeps += poly
    this

  /** Complete any reverse dependency indexing deferred by `init`. */
  private def materializeDeps()(using Context): this.type =
    if !dirtyDeps.isEmpty then
      val toIndex = dirtyDeps
      dirtyDeps = SimpleIdentitySet.empty
      toIndex.foreach { poly =>
        val entries = boundsMap(poly)
        if entries != null then adjustDeps(poly, entries, add = true)
      }
    this

  override def dependsOn(tv: TypeVar, except: TypeVars, co: Boolean)(using Context): Boolean =
    materializeDeps()
    def origin(tv: TypeVar) =
      assert(!instType(tv).exists)
      tv.origin
    val param = origin(tv)
    val excluded = except.map(origin)
    val qualifies: TypeParamRef => Boolean = !excluded.contains(_)
    if co then
      coDeps.at(param).exists(qualifies) || upper(param).exists(qualifies)
    else
      contraDeps.at(param).exists(qualifies) || lower(param).exists(qualifies)

  /** Modify traversals in two respects:
   *   - when encountering an application C[Ts], where C is a type variable or parameter
   *     that has an instantiation in this constraint, assume the type parameters of
   *     the instantiation instead of the type parameters of C when traversing the
   *     arguments Ts. That can make a difference for the variance in which an argument
   *     is traversed. Example constraint:
   *
   *         constrained types: C[X], A
   *         A >: C[B]
   *         C := Option
   *
   *     Here, B is traversed with variance +1 instead of 0. Test case: pos/t3152.scala
   *
   *   - When typing a prefx, don't avoid negative variances. This matters only for the
   *     corner case where a parameter is instantiated to Nothing (see comment in
   *     TypeAccumulator#applyToPrefix). When determining instantiation directions in
   *     interpolations (which is what dependency variances are for), it can be ignored.
   */
  private trait ConstraintAwareTraversal[T] extends TypeAccumulator[T]:

    /** Does `param` have bounds in the current constraint? */
    protected def hasBounds(param: TypeParamRef, paramEntry: Type): Boolean = paramEntry.isInstanceOf[TypeBounds]

    protected def hasBounds(param: TypeParamRef): Boolean = hasBounds(param, entry(param))

    override def tyconTypeParams(tp: AppliedType)(using Context): List[ParamInfo] =
      def tparams(tycon: Type): List[ParamInfo] = tycon match
        case tycon: TypeVar if !tycon.isPermanentlyInstantiated => tparams(tycon.origin)
        case tycon: TypeParamRef =>
          val tyconEntry = entry(tycon)
          if !hasBounds(tycon, tyconEntry) then
            val entryParams = tyconEntry.typeParams
            if entryParams.nonEmpty then entryParams
            else tp.tyconTypeParams
          else tp.tyconTypeParams
        case _ => tp.tyconTypeParams
      tparams(tp.tycon)

    override def applyToPrefix(x: T, tp: NamedType): T =
      this(x, tp.prefix)
  end ConstraintAwareTraversal

  /** A type traverser that adjust dependencies originating from a given type
   *  @param ignoreBinding  if not null, a parameter that is assumed to be still uninstantiated.
   *                        This is necessary to handle replacements.
   */
  private class Adjuster(initialSrcParam: TypeParamRef, initialIgnoreBinding: TypeParamRef | Null)(using Context)
  extends TypeTraverser, ConstraintAwareTraversal[Unit]:

    private var srcParam: TypeParamRef = initialSrcParam
    private var ignoreBinding: TypeParamRef | Null = initialIgnoreBinding
    var add: Boolean = compiletime.uninitialized

    def reset(srcParam: TypeParamRef, ignoreBinding: TypeParamRef | Null): this.type =
      this.srcParam = srcParam
      this.ignoreBinding = ignoreBinding
      this

    private class SeenLazyRefs:
      private var used: Int = 0
      private var keys: Array[LazyRef | Null] = new Array[LazyRef | Null](8)
      private var stamps: Array[Int] = new Array[Int](8)
      private var limit: Int = keys.length - (keys.length >> 2)

      def reset(): Unit =
        var i = 0
        while i < keys.length do
          keys(i) = null
          i += 1
        used = 0

      private def index(ref: LazyRef): Int =
        val h = System.identityHashCode(ref)
        val i = (h ^ (h >>> 16)) * 0x85EBCA6B
        val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
        j & (keys.length - 1)

      private def insertKnown(ref: LazyRef, stamp: Int): Unit =
        var idx = index(ref)
        while keys(idx) != null do idx = (idx + 1) & (keys.length - 1)
        keys(idx) = ref
        stamps(idx) = stamp
        used += 1

      private def grow(): Unit =
        val oldKeys = keys
        val oldStamps = stamps
        keys = new Array[LazyRef | Null](oldKeys.length << 1)
        stamps = new Array[Int](keys.length)
        limit = keys.length - (keys.length >> 2)
        used = 0
        var i = 0
        while i < oldKeys.length do
          val ref = oldKeys(i)
          if ref != null then insertKnown(ref, oldStamps(i))
          i += 1

      def addIfNew(ref: LazyRef, generation: Int): Boolean =
        var idx = index(ref)
        while keys(idx) != null do
          if keys(idx).nn eq ref then
            if stamps(idx) == generation then return false
            stamps(idx) = generation
            return true
          idx = (idx + 1) & (keys.length - 1)
        if used >= limit then
          grow()
          idx = index(ref)
          while keys(idx) != null do idx = (idx + 1) & (keys.length - 1)
        keys(idx) = ref
        stamps(idx) = generation
        used += 1
        true

    private var seen: SeenLazyRefs | Null = null
    var seenGeneration: Int = 0

    def resetSeen(): Unit =
      val seenRefs = seen
      if seenRefs != null then seenRefs.reset()

    private def seenLazyRefs: SeenLazyRefs =
      val seenRefs = seen
      if seenRefs == null then
        val fresh = SeenLazyRefs()
        seen = fresh
        fresh
      else seenRefs

    override protected def hasBounds(param: TypeParamRef) =
      (param eq ignoreBinding) || super.hasBounds(param)
    override protected def hasBounds(param: TypeParamRef, paramEntry: Type) =
      (param eq ignoreBinding) || super.hasBounds(param, paramEntry)

    def update(deps: ReverseDeps, referenced: TypeParamRef): ReverseDeps =
      val prev = deps.at(referenced)
      val newSet = if add then prev + srcParam else prev - srcParam
      if newSet.isEmpty then deps.remove(referenced)
      else deps.updated(referenced, newSet)

    def traverse(t: Type) = try
      t match
      case param: TypeParamRef =>
        if hasBounds(param) then
          if variance >= 0 then coDeps = update(coDeps, param)
          if variance <= 0 then contraDeps = update(contraDeps, param)
        else
          traverse(entry(param))
      case tp: LazyRef =>
        if seenLazyRefs.addIfNew(tp, seenGeneration) then
          traverse(tp.ref)
      case _ => traverseChildren(t)
    catch case ex: Throwable => handleRecursive("adjust", t.show, ex)
  end Adjuster

  /** Adjust dependencies to account for the delta of previous entry `prevEntry`
   *  and the new bound `entry` for the type parameter `srcParam`.
   */
  def adjustDeps(entry: Type | Null, prevEntry: Type | Null, srcParam: TypeParamRef, ignoreBinding: TypeParamRef | Null = null)(using Context): this.type =
    var myAdjuster: Adjuster | Null = null

    def adjuster: Adjuster =
      val a = myAdjuster
      if a == null then
        val fresh = new Adjuster(srcParam, ignoreBinding)
        myAdjuster = fresh
        fresh
      else a

    /** Adjust reverse dependencies of all type parameters referenced by `bound`
     *  @param  isLower `bound` is a lower bound
     *  @param  add     if true, add referenced variables to dependencies, otherwise drop them.
     */
    def adjustReferenced(bound: Type, isLower: Boolean, add: Boolean) =
      adjuster.variance = if isLower then 1 else -1
      adjuster.add = add
      adjuster.seenGeneration += 1
      if adjuster.seenGeneration == 0 then
        adjuster.resetSeen()
        adjuster.seenGeneration = 1
      adjuster.traverse(bound)

    /** Use an optimized strategy to adjust dependencies to account for the delta
     *  of previous bound `prevBound` and new bound `bound`: If `prevBound` is some
     *  and/or prefix of `bound`, and `baseCase` is true, just add the new parts of `bound`.
     *  @param  isLower `bound` and `prevBound` are lower bounds
     *  @return true iff the delta strategy succeeded, false if it failed in which case
     *          the constraint is left unchanged.
     */
    def adjustDelta(bound: Type, prevBound: Type, isLower: Boolean, baseCase: => Boolean): Boolean =
      if bound eq prevBound then
        baseCase
      else bound match
        case bound: AndOrType =>
          adjustDelta(bound.tp1, prevBound, isLower, baseCase) && {
            adjustReferenced(bound.tp2, isLower, add = true)
            true
          }
        case _ => false

    /** Add or remove dependencies referenced in `bounds`.
     *  @param add   if true, dependencies are added, otherwise they are removed
     */
    def adjustBounds(bounds: TypeBounds, add: Boolean) =
      adjustReferenced(bounds.lo, isLower = true, add)
      adjustReferenced(bounds.hi, isLower = false, add)

    entry match
      case entry @ TypeBounds(lo, hi) =>
        prevEntry match
          case prevEntry @ TypeBounds(plo, phi) =>
            if !adjustDelta(lo, plo, isLower = true,
                  adjustDelta(hi, phi, isLower = false, true))
            then
              adjustBounds(prevEntry, add = false)
              adjustBounds(entry, add = true)
          case _ =>
            adjustBounds(entry, add = true)
      case _ =>
        prevEntry match
          case prevEntry: TypeBounds =>
            adjustBounds(prevEntry, add = false)
          case _ =>
        dropDeps(srcParam) // srcParam is instantiated, so its dependencies can be dropped
    this
  end adjustDeps

  /** Adjust dependencies to account for adding or dropping all `entries` associated
   *  with `poly`.
   *  @param add   if true, entries is added, otherwise it is dropped
   */
  def adjustDeps(poly: TypeLambda, entries: Array[Type], add: Boolean)(using Context): this.type =
    val limit = paramCount(entries)
    var myAdjuster: Adjuster | Null = null

    def adjuster(srcParam: TypeParamRef): Adjuster =
      val a = myAdjuster
      if a == null then
        val fresh = new Adjuster(srcParam, null)
        myAdjuster = fresh
        fresh
      else a.reset(srcParam, null)

    def adjustReferenced(adjuster: Adjuster, bound: Type, isLower: Boolean, add: Boolean) =
      adjuster.variance = if isLower then 1 else -1
      adjuster.add = add
      adjuster.seenGeneration += 1
      if adjuster.seenGeneration == 0 then
        adjuster.resetSeen()
        adjuster.seenGeneration = 1
      adjuster.traverse(bound)

    def adjustBounds(bounds: TypeBounds, add: Boolean, srcParam: TypeParamRef) =
      val a = adjuster(srcParam)
      adjustReferenced(a, bounds.lo, isLower = true, add)
      adjustReferenced(a, bounds.hi, isLower = false, add)

    var n = 0
    while n < limit do
      val param = poly.paramRef(n)
      entries(n) match
        case bounds: TypeBounds =>
          adjustBounds(bounds, add, param)
          if !add then dropDeps(param)
        case _ =>
          dropDeps(param)
      n += 1
    this

  /** Remove all reverse dependencies of `param` */
  def dropDeps(param: TypeParamRef)(using Context): Unit =
    coDeps = coDeps.remove(param)
    contraDeps = contraDeps.remove(param)

  /** A string representing the two dependency maps */
  def depsToString(using Context): String =
    materializeDeps()
    def depsStr(deps: ReverseDeps): String =
      def depStr(param: TypeParamRef) = i"$param --> ${deps.at(param).toList}%, %"
      if deps.isEmpty then "" else i"\n     ${deps.toList.map((k, v) => depStr(k))}%\n     %"
    i" co-deps:${depsStr(coDeps)}\n contra-deps:${depsStr(contraDeps)}\n"

// ---------- Adding TypeLambdas --------------------------------------------------

  /** The bound type `tp` without constrained parameters which are clearly
   *  dependent. A parameter in an upper bound is clearly dependent if it appears
   *  in a hole of a context H given by:
   *
   *      H = []
   *          H & T
   *          T & H
   *
   *  (the idea is that a parameter P in a H context is guaranteed to be a supertype of the
   *   bounded parameter.)
   *  Analogously, a parameter in a lower bound is clearly dependent if it appears
   *  in a hole of a context H given by:
   *
   *      L = []
   *          L | T
   *          T | L
   *
   *  "Clearly dependent" is not synonymous with "dependent" in the sense
   *  it is defined in `dependentParams`. Dependent parameters are handled
   *  in `updateEntry`. The idea of stripping off clearly dependent parameters
   *  and to handle them separately is for efficiency, so that type expressions
   *  used as bounds become smaller.
   *
   *  TODO: try to do without stripping? It would mean it is more efficient
   *  to pull out full bounds from a constraint.
   *
   *  @param isUpper   If true, `bound` is an upper bound, else a lower bound.
   */
  private final class StripParamTodos:
    private var params = new Array[TypeParamRef | Null](4)
    private var isUppers = new Array[Boolean](4)
    private var mySize = 0

    def mark: Int = mySize

    def add(isUpper: Boolean, param: TypeParamRef): Unit =
      if mySize == params.length then
        val params1 = new Array[TypeParamRef | Null](mySize * 2)
        val isUppers1 = new Array[Boolean](mySize * 2)
        Array.copy(params, 0, params1, 0, mySize)
        Array.copy(isUppers, 0, isUppers1, 0, mySize)
        params = params1
        isUppers = isUppers1
      params(mySize) = param
      isUppers(mySize) = isUpper
      mySize += 1

    def rollback(mark: Int): Unit =
      var i = mark
      while i < mySize do
        params(i) = null
        i += 1
      mySize = mark

    def applyTo(current: OrderingConstraint, param: TypeParamRef)(using Context): OrderingConstraint =
      var current1 = current
      var i = 0
      while i < mySize do
        val depParam = params(i).nn
        current1 =
          if isUppers(i) then order(current1, param, depParam)
          else order(current1, depParam, param)
        params(i) = null
        i += 1
      mySize = 0
      current1
  end StripParamTodos

  private final class StripParamsMap(todos: StripParamTodos)(using Context) extends AvoidWildcardsMap:
    private var myMapped: Type = NoType

    private def stripMapped(tp: Type, isUpper: Boolean)(using Context): Type = tp match
      case param: TypeParamRef if contains(param) =>
        todos.add(isUpper, param)
        NoType
      case tp: TypeBounds =>
        val lo1 = stripMapped(tp.lo, !isUpper).orElse(defn.NothingType)
        val hi1 = stripMapped(tp.hi, isUpper).orElse(tp.topType)
        tp.derivedTypeBounds(lo1, hi1)
      case tp: AndType if isUpper =>
        val tp1 = stripMapped(tp.tp1, isUpper)
        val tp2 = stripMapped(tp.tp2, isUpper)
        if tp1.exists then
          if tp2.exists then tp.derivedAndType(tp1, tp2)
          else tp1
        else tp2
      case tp: OrType if !isUpper =>
        val tp1 = stripMapped(tp.tp1, isUpper)
        val tp2 = stripMapped(tp.tp2, isUpper)
        if tp1.exists then
          if tp2.exists then tp.derivedOrType(tp1, tp2)
          else tp1
        else tp2
      case _ =>
        tp

    private def stripFallback(tp: Type, isUpper: Boolean, mark: Int): Type =
      todos.rollback(mark)
      stripMapped(tp, isUpper)

    /** The result of `stripMapped(apply(tp), isUpper)`.
     *
     *  `myMapped` is set to `apply(tp)` for callers that need to rebuild the
     *  wildcard-approximated parent before deciding whether its stripped
     *  children are semantically reachable.
     */
    def strip(tp: Type, isUpper: Boolean)(using Context): Type = tp match
      case param: TypeParamRef if contains(param) =>
        myMapped = param
        todos.add(isUpper, param)
        NoType

      case tp: TypeBounds =>
        val mark = todos.mark
        val saved = variance
        variance = -saved
        val lo1 = strip(tp.lo, !isUpper)
        val loMapped = myMapped
        variance = saved
        val hi1 = strip(tp.hi, isUpper)
        val hiMapped = myMapped
        val mapped = derivedTypeBounds(tp, loMapped, hiMapped)
        myMapped = mapped
        mapped match
          case mapped: TypeBounds if (mapped.lo eq loMapped) && (mapped.hi eq hiMapped) =>
            mapped.derivedTypeBounds(lo1.orElse(defn.NothingType), hi1.orElse(mapped.topType))
          case _ =>
            stripFallback(mapped, isUpper, mark)

      case tp: AndType if isUpper =>
        val mark = todos.mark
        val tp1 = strip(tp.tp1, isUpper)
        val tp1Mapped = myMapped
        val tp2 = strip(tp.tp2, isUpper)
        val tp2Mapped = myMapped
        val mapped = derivedAndType(tp, tp1Mapped, tp2Mapped)
        myMapped = mapped
        mapped match
          case mapped: AndType if (mapped.tp1 eq tp1Mapped) && (mapped.tp2 eq tp2Mapped) =>
            if tp1.exists then
              if tp2.exists then mapped.derivedAndType(tp1, tp2)
              else tp1
            else tp2
          case _ =>
            stripFallback(mapped, isUpper, mark)

      case tp: OrType if !isUpper =>
        val mark = todos.mark
        val tp1 = strip(tp.tp1, isUpper)
        val tp1Mapped = myMapped
        val tp2 = strip(tp.tp2, isUpper)
        val tp2Mapped = myMapped
        val mapped = derivedOrType(tp, tp1Mapped, tp2Mapped)
        myMapped = mapped
        mapped match
          case mapped: OrType if (mapped.tp1 eq tp1Mapped) && (mapped.tp2 eq tp2Mapped) =>
            if tp1.exists then
              if tp2.exists then mapped.derivedOrType(tp1, tp2)
              else tp1
            else tp2
          case _ =>
            stripFallback(mapped, isUpper, mark)

      case _ =>
        val mapped = apply(tp)
        myMapped = mapped
        stripMapped(mapped, isUpper)
  end StripParamsMap

  def add(poly: TypeLambda, tvars: List[TypeVar])(using Context): This = {
    assert(!contains(poly))
    val nparams = poly.paramNames.length
    val entries1 = new Array[Type](nparams * 2)
    poly.paramInfos.copyToArray(entries1, 0)
    tvars.copyToArray(entries1, nparams)
    newConstraint(boundsMap = this.boundsMap.updated(poly, entries1))
      .init(poly)
  }

  /** Split dependent parameters off the bounds for parameters in `poly`.
   *  Update all bounds to be normalized and update ordering to account for
   *  dependent parameters.
   */
  private def init(poly: TypeLambda)(using Context): This = {
    var current = this
    val todos = StripParamTodos()
    val stripParams = StripParamsMap(todos)
    // Iterate `paramRefs` directly: the previous `while (i < paramNames.length)`
    // form recomputed `paramNames.length` (O(n) on a List) and `paramRefs(i)`
    // (O(i)) every iteration, making the loop O(n^2).
    var refs = poly.paramRefs
    while refs ne Nil do
      val param = refs.head
      val stripped = stripParams.strip(nonParamBounds(param), isUpper = true)
      current = boundsLens.update(this, current, param, stripped)
      current = todos.applyTo(current, param)
      refs = refs.tail
    current.markDepsDirty(poly)
      .checkWellFormed()
  }

// ---------- Updates ------------------------------------------------------------

  def validBoundFor(param: TypeParamRef, bound: Type, isUpper: Boolean)(using Context): Type =
    def recur(tp: Type): Type = tp match
      case tp: AndOrType =>
        val r1 = recur(tp.tp1)
        val r2 = recur(tp.tp2)
        if (r1 eq tp.tp1) && (r2 eq tp.tp2) then tp
        else tp.match
          case tp: OrType =>
            TypeComparer.lub(r1, r2, isSoft = tp.isSoft)
          case _ =>
            r1 & r2
      case tp: TypeParamRef =>
        if tp eq param then
          if isUpper then defn.AnyType else defn.NothingType
        else entry(tp) match
          case NoType => tp
          case TypeBounds(lo, hi) => if lo eq hi then recur(lo) else tp
          case inst => recur(inst)
      case tp: TypeVar =>
        val underlying1 = recur(tp.underlying)
        if underlying1 ne tp.underlying then underlying1 else tp
      case CapturingType(parent, refs) =>
        tp.derivedCapturingType(recur(parent), refs)
      case tp: FlexibleType =>
        tp.derivedFlexibleType(recur(tp.hi))
      case tp: AnnotatedType =>
        tp.derivedAnnotatedType(recur(tp.parent), tp.annot)
      case _ =>
        val tp1 = tp.dealiasKeepAnnots
        if tp1 ne tp then
          val tp2 = recur(tp1)
          if tp2 ne tp1 then tp2 else tp
        else tp

    recur(bound)
  end validBoundFor

  def validBoundsFor(param: TypeParamRef, bounds: TypeBounds)(using Context): Type =
    bounds.derivedTypeBounds(
      validBoundFor(param, bounds.lo, isUpper = false),
      validBoundFor(param, bounds.hi, isUpper = true))

  /** Add the fact `param1 <: param2` to the constraint `current` and propagate
   *  `<:<` relationships between parameters ("edges") but not bounds.
   */
  def order(current: This, param1: TypeParamRef, param2: TypeParamRef, direction: UnificationDirection = NoUnification)(using Context): This =
    // /!\ Careful here: we're adding constraints on `current`, not `this`, so
    // think twice when using an instance method! We only need to pass `this` as
    // the `prev` argument in methods on `ConstraintLens`.
    // TODO: Refactor this code to take `prev` as a parameter and add
    // constraints on `this` instead?
    if param1 == param2 || current.isLess(param1, param2) then current
    else
      assert(current.contains(param1), i"$param1")
      assert(current.contains(param2), i"$param2")
      val unifying = direction != NoUnification
      val newUpper = {
        val up = current.exclusiveUpper(param2, param1)
        if unifying then
          // Since param2 <:< param1 already holds now, filter out param1 to avoid adding
          //   duplicated orderings.
          val filtered = up.filterNot(_ eq param1)
          // Only add bounds for param2 if it will be kept in the constraint after unification.
          if direction == KeepParam2 then
            param2 :: filtered
          else
            filtered
        else
          param2 :: up
      }
      val newLower = {
        val lower = current.exclusiveLower(param1, param2)
        if unifying then
          // Similarly, filter out param2 from lowerly-ordered parameters
          //   to avoid duplicated orderings.
          val filtered = lower.filterNot(_ eq param2)
          // Only add bounds for param1 if it will be kept in the constraint after unification.
          if direction == KeepParam1 then
            param1 :: filtered
          else
            filtered
        else
          param1 :: lower
      }
      val current1 = newLower.foldLeft(current)(upperLens.map(this, _, _, newUpper ::: _))
      val current2 = newUpper.foldLeft(current1)(lowerLens.map(this, _, _, newLower ::: _))
      current2
    end if
  end order

  /** The list of parameters P such that, for a fresh type parameter Q:
   *
   *    Q <: tp  implies  Q <: P      and isUpper = true, or
   *    tp <: Q  implies  P <: Q      and isUpper = false
   */
  private def dependentParams(tp: Type, isUpper: Boolean)(using Context): List[TypeParamRef] = tp match
    case param: TypeParamRef if contains(param) =>
      param :: (if (isUpper) upper(param) else lower(param))
    case tp: AndType if isUpper  =>
      dependentParams(tp.tp1, isUpper).setUnion(dependentParams(tp.tp2, isUpper))
    case tp: OrType if !isUpper =>
      dependentParams(tp.tp1, isUpper).intersect(dependentParams(tp.tp2, isUpper))
    case EtaExpansion(tycon) =>
      dependentParams(tycon, isUpper)
    case _ =>
      Nil

  private def updateEntry(current: This, param: TypeParamRef, newEntry: Type)(using Context): This = {
    if Config.checkNoWildcardsInConstraint then assert(!newEntry.containsWildcardTypes)
    val oldEntry = current.entry(param)
    var current1 = boundsLens.update(this, current, param, newEntry)
      .adjustDeps(newEntry, oldEntry, param)
    newEntry match {
      case TypeBounds(lo, hi) =>
        for p <- dependentParams(lo, isUpper = false) do
          current1 = order(current1, p, param)
        for p <- dependentParams(hi, isUpper = true) do
          current1 = order(current1, param, p)
      case _ =>
    }
    current1
  }

  def updateEntry(param: TypeParamRef, tp: Type)(using Context): This =
    materializeDeps()
    updateEntry(this, param, tp).checkWellFormed()

  def addLess(param1: TypeParamRef, param2: TypeParamRef, direction: UnificationDirection)(using Context): This =
    order(this, param1, param2, direction).checkWellFormed()

// ---------- Replacements and Removals -------------------------------------

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   */
  def replace(param: TypeParamRef, tp: Type)(using Context): OrderingConstraint =
    materializeDeps()
    val replacement = tp.dealiasKeepAnnots.stripTypeVar
    if param == replacement then this.checkWellFormed()
    else
      assert(replacement.isValueTypeOrLambda)

      val replacedTypeVar = typeVarOfParam(param)
      //println(i"replace $param with $replacement in $this")

      def mapReplacedTypeVarTo(to: Type) = new TypeMap:
        override def apply(t: Type): Type =
          if (t eq replacedTypeVar) && t.exists then to else mapOver(t)

      val coDepsOfParam = coDeps.at(param)
      val contraDepsOfParam = contraDeps.at(param)

      var current = updateEntry(this, param, replacement)
        // Need to update param early to avoid infinite recursion on instantiation.
        // See i16311.scala for a test case. On the other hand, for the purposes of
        // dependency adjustment, we need to pretend that `param` is still unbound.
        // We achieve that by passing a `ignoreBinding = param` to `adjustDeps` below.

      def removeParamFrom(ps: List[TypeParamRef]) =
        ps.filterConserve(param ne _)

      for lo <- lower(param) do
        current = upperLens.map(this, current, lo, removeParamFrom)
      for hi <- upper(param) do
        current = lowerLens.map(this, current, hi, removeParamFrom)

      def replaceParamIn(other: TypeParamRef) =
        val oldEntry = current.entry(other)
        val newEntry = oldEntry.substParam(param, replacement) match
          case tp: TypeBounds => current.validBoundsFor(other, tp)
          case tp => tp
        current = boundsLens.update(this, current, other, newEntry)
        var oldDepEntry = oldEntry
        var newDepEntry = newEntry
        replacedTypeVar match
          case tvar: TypeVar =>
            if tvar.isPermanentlyInstantiated // `isInstantiated` would use ctx.typerState.constraint rather than the current constraint
            then
              // If the type variable has been instantiated, we need to forget about
              // the instantiation for old dependencies.
              // I.e. to find out what the old entry was, we should not follow
              // the newly instantiated type variable but assume the type variable's origin `param`.
              // An example where this happens is if `replace` is called from TypeVar's `instantiateWith`.
              oldDepEntry = mapReplacedTypeVarTo(param)(oldDepEntry)
            else
              // If the type variable has not been instantiated, we need to replace references to it
              // in the new entry by `replacement`. Otherwise we would get stuck in an uninstantiated
              // type variable.
              // An example where this happens is if `replace` is called from unify.
              newDepEntry = mapReplacedTypeVarTo(replacement)(newDepEntry)
          case _ =>
        if oldDepEntry ne newDepEntry then
          current.adjustDeps(newDepEntry, oldDepEntry, other, ignoreBinding = param)
      end replaceParamIn

      if optimizeReplace then
        current.foreachParam { (p, i) =>
          val other = p.paramRefs(i)
          entry(other) match
            case _: TypeBounds =>
              if coDepsOfParam.contains(other) || contraDepsOfParam.contains(other) then
                replaceParamIn(other)
            case _ => replaceParamIn(other)
        }
      else
        current.foreachParam { (p, i) =>
          val other = p.paramRefs(i)
          if other != param then replaceParamIn(other)
        }
      if isRemovable(param.binder) then current = current.remove(param.binder)
      current.dropDeps(param)
      replacedTypeVar match
        case replacedTypeVar: TypeVar if isHard(replacedTypeVar) => current = current.hardenTypeVars(replacement)
        case _ =>
      current.checkWellFormed()
  end replace

  def hardenTypeVars(tp: Type)(using Context): OrderingConstraint = tp.dealiasKeepRefiningAnnots match
    case tp: TypeVar if contains(tp.origin) => withHard(tp)
    case tp: TypeParamRef if contains(tp)   => hardenTypeVars(typeVarOfParam(tp))
    case tp: AndOrType                      => hardenTypeVars(tp.tp1).hardenTypeVars(tp.tp2)
    case tp: FlexibleType                   => hardenTypeVars(tp.hi)
    case _                                  => this

  def remove(pt: TypeLambda)(using Context): This = {
    materializeDeps()
    def removeFromOrdering(po: ParamOrdering) = {
      def removeFromBoundss(key: TypeLambda, bndss: Array[List[TypeParamRef]]): Array[List[TypeParamRef]] = {
        val bndss1 = bndss.map(_.filterConserve(_.binder ne pt))
        if (bndss.corresponds(bndss1)(_ eq _)) bndss else bndss1
      }
      po.remove(pt).mapValuesNow(removeFromBoundss)
    }
    val hardVars1 = pt.paramRefs.foldLeft(hardVars)((hvs, param) => hvs - typeVarOfParam(param))
    newConstraint(boundsMap.remove(pt), removeFromOrdering(lowerMap), removeFromOrdering(upperMap), hardVars1)
      .adjustDeps(pt, boundsMap(pt).nn, add = false)
      .checkWellFormed()
  }

  def isRemovable(pt: TypeLambda): Boolean = {
    val entries = boundsMap(pt).nn
    @tailrec def allRemovable(last: Int): Boolean =
      if (last < 0) true
      else typeVar(entries, last) match {
        case tv: TypeVar => tv.isPermanentlyInstantiated && allRemovable(last - 1)
        case _ => false
      }
    allRemovable(paramCount(entries) - 1)
  }

// ----------- Joins -----------------------------------------------------

  def hasConflictingTypeVarsFor(tl: TypeLambda, that: Constraint): Boolean =
    contains(tl) && that.contains(tl) &&
    // Since TypeVars are allocated in bulk for each type lambda, we only have
    // to check the first one to find out if some of them are different.
    (this.typeVarOfParam(tl.paramRefs(0)) ne that.typeVarOfParam(tl.paramRefs(0)))

  def subst(from: TypeLambda, to: TypeLambda)(using Context): OrderingConstraint =
    materializeDeps()
    def swapKey[T](m: ArrayValuedMap[T]) =
      val info = m(from)
      if info == null then m else m.remove(from).updated(to, info)
    var current = newConstraint(swapKey(boundsMap), swapKey(lowerMap), swapKey(upperMap))
    def subst[T <: Type](x: T): T = x.subst(from, to).asInstanceOf[T]
    current.foreachParam {(p, i) =>
      current = boundsLens.map(this, current, p, i, subst)
      current = lowerLens.map(this, current, p, i, _.map(subst))
      current = upperLens.map(this, current, p, i, _.map(subst))
    }
    constr.println(i"renamed $this to $current")
    current.checkWellFormed()

  def isHard(tv: TypeVar) = hardVars.contains(tv)

  def withHard(tv: TypeVar)(using Context) =
    newConstraint(hardVars = this.hardVars + tv)

  def instType(tvar: TypeVar): Type = entry(tvar.origin) match
    case _: TypeBounds => NoType
    case tp: TypeParamRef => typeVarOfParam(tp).orElse(tp)
    case tp => tp

  def ensureFresh(tl: TypeLambda)(using Context): TypeLambda =
    if (contains(tl)) {
      var paramInfos = tl.paramInfos
      if (tl.isInstanceOf[HKLambda]) {
        // HKLambdas are hash-consed, need to create an artificial difference by adding
        // a LazyRef to a bound.
        val TypeBounds(lo, hi) :: pinfos1 = tl.paramInfos: @unchecked
        paramInfos = TypeBounds(lo, LazyRef.of(hi)) :: pinfos1
      }
      ensureFresh(tl.newLikeThis(tl.paramNames, paramInfos, tl.resultType))
    }
    else tl

  def checkConsistentVars()(using Context): Unit =
    for param <- domainParams do
      typeVarOfParam(param) match
        case tvar: TypeVar =>
          assert(tvar.origin == param, i"mismatch $tvar, $param")
        case _ =>

  def occursAtToplevel(param: TypeParamRef, inst: Type)(using Context): Boolean =
    def occurs(tp: Type)(using Context): Boolean = tp match
      case tp: AndOrType =>
        occurs(tp.tp1) || occurs(tp.tp2)
      case tp: TypeParamRef =>
        (tp eq param) || entry(tp).match
          case NoType => false
          case TypeBounds(lo, hi) => (lo eq hi) && occurs(lo)
          case inst => occurs(inst)
      case tp: TypeVar =>
        occurs(tp.underlying)
      case TypeBounds(lo, hi) =>
        occurs(lo) || occurs(hi)
      case _ =>
        val tp1 = tp.dealias
        (tp1 ne tp) && occurs(tp1)

    occurs(inst)
  end occursAtToplevel

// ---------- Exploration --------------------------------------------------------

  def domainLambdas: List[TypeLambda] = boundsMap.keys

  def domainParams: List[TypeParamRef] =
    for {
      (poly, entries) <- boundsMap.toList
      n <- 0 until paramCount(entries)
      if entries(n).exists
    }
    yield poly.paramRefs(n)

  def forallParams(p: TypeParamRef => Boolean): Boolean =
    boundsMap.forallBinding { (poly, entries) =>
      !0.until(paramCount(entries)).exists(i => isBounds(entries(i)) && !p(poly.paramRefs(i)))
    }

  def foreachParam(p: (TypeLambda, Int) => Unit): Unit =
    boundsMap.foreachBinding { (poly, entries) =>
      0.until(poly.paramNames.length).foreach(p(poly, _))
    }

  def foreachTypeVar(op: TypeVar => Unit): Unit =
    boundsMap.foreachBinding { (poly, entries) =>
      var i = 0
      val limit = paramCount(entries)
      while i < limit do
        typeVar(entries, i) match
          case tv: TypeVar if !tv.isPermanentlyInstantiated => op(tv)
          case _ =>
        i += 1
    }

  private var myUninstVars: mutable.ArrayBuffer[TypeVar] | Null = null

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar] =
    myUninstVars match
      case uv: mutable.ArrayBuffer[TypeVar] if !uv.exists(_.isPermanentlyInstantiated) => uv
      case _ =>
        val res = new mutable.ArrayBuffer[TypeVar]
        myUninstVars = res
        boundsMap.foreachBinding { (poly, entries) =>
          for (i <- 0 until paramCount(entries))
            typeVar(entries, i) match {
              case tv: TypeVar if !tv.isPermanentlyInstantiated && isBounds(entries(i)) => res += tv
              case _ =>
            }
        }
        res

// ---------- Checking -----------------------------------------------

  def checkWellFormed()(using Context): this.type =

    /** Check that each dependency A -> B in coDeps and contraDeps corresponds to
     *  a reference to A at the right variance in the entry of B.
     */
    def checkBackward(deps: ReverseDeps, depsName: String, v: Int)(using Context): Unit =
      deps.foreachBinding { (param, params) =>
        for srcParam <- params do
          assert(contains(srcParam) && occursAtVariance(param, v, in = entry(srcParam)),
            i"wrong $depsName backwards reference $param -> $srcParam in $thisConstraint")
      }

    /** A type traverser that checks that all references bound in the constraint
     *  are accounted for in coDeps and/or contraDeps.
     */
    def checkForward(srcParam: TypeParamRef)(using Context) =
      new TypeTraverser with ConstraintAwareTraversal[Unit]:
        val seen = util.HashSet[LazyRef]()
        def traverse(t: Type): Unit = t match
          case param: TypeParamRef if param ne srcParam =>
            def check(deps: ReverseDeps, directDeps: List[TypeParamRef], depsName: String) =
              assert(deps.at(param).contains(srcParam) || directDeps.contains(srcParam),
                i"missing $depsName backwards reference $param -> $srcParam in $thisConstraint")
            entry(param) match
              case _: TypeBounds =>
                if variance >= 0 then check(contraDeps, upper(param), "contra")
                if variance <= 0 then check(coDeps, lower(param), "co")
              case tp =>
                traverse(tp)
          case tp: LazyRef =>
            if !seen.contains(tp) then
              seen += tp
              traverse(tp.ref)
          case _ => traverseChildren(t)

    /** Does `param` occur at variance `v` or else at variance 0 in entry `in`? */
    def occursAtVariance(param: TypeParamRef, v: Int, in: Type)(using Context): Boolean =
      val test = new TypeAccumulator[Boolean] with ConstraintAwareTraversal[Boolean]:
        def apply(x: Boolean, t: Type): Boolean =
          if x then true
          else t match
            case t: TypeParamRef =>
              entry(t) match
                case _: TypeBounds =>
                  t == param && (variance == 0 || variance == v)
                case e =>
                  apply(x, e)
            case _ =>
              foldOver(x, t)
      test(false, in)

    if Config.checkConstraintsNonCyclic then
      domainParams.foreach { param =>
        val inst = entry(param)
        assert(!isLess(param, param),
          s"cyclic ordering involving $param in ${this.show}, upper = $inst")
        assert(!occursAtToplevel(param, inst),
          s"cyclic bound for $param: ${inst.show} in ${this.show}")
      }
    if Config.checkConstraintDeps || ctx.settings.YcheckConstraintDeps.value then
      materializeDeps()
      checkBackward(coDeps, "co", -1)
      checkBackward(contraDeps, "contra", +1)
      domainParams.foreach(p => if contains(p) then checkForward(p).traverse(entry(p)))

    this
  end checkWellFormed

  override def checkClosed()(using Context): Unit =

    def isFreeTypeParamRef(tp: Type) = tp match
      case TypeParamRef(binder: TypeLambda, _) => !contains(binder)
      case _ => false

    def checkClosedType(tp: Type | Null, where: String) =
      if tp != null then
        assert(!tp.existsPart(isFreeTypeParamRef), i"unclosed constraint: $this refers to $tp in $where")

    boundsMap.foreachBinding((_, tps) => tps.foreach(checkClosedType(_, "bounds")))
    lowerMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "lower"))))
    upperMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "upper"))))
  end checkClosed

// ---------- Printing -----------------------------------------------------

  override def toText(printer: Printer): Text =
    printer.toText(this)

  override def toString: String = {
    def entryText(tp: Type): String = tp match {
      case tp: TypeBounds => tp.toString
      case _ => " := " + tp
    }
    val constrainedText =
      " constrained types = " + domainLambdas.mkString("\n")
    val boundsText =
      "\n bounds = " + {
        val assocs =
          for (param <- domainParams)
          yield
            s"${param.binder.paramNames(param.paramNum)}: ${entryText(entry(param))}"
        assocs.mkString("\n")
    }
    val depsText =
      "\n coDeps = " + coDeps +
      "\n contraDeps = " + contraDeps
    constrainedText + boundsText + depsText
  }
}
