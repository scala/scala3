package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Decorators._, TypeApplications._
import util.SimpleIdentityMap
import collection.mutable
import printing.Printer
import printing.Texts._
import config.Config
import config.Printers.constr
import reflect.ClassTag
import annotation.tailrec
import annotation.internal.sharable

object OrderingConstraint {

  type ArrayValuedMap[T] = SimpleIdentityMap[TypeLambda, Array[T]]

  /** The type of `OrderingConstraint#boundsMap` */
  type ParamBounds = ArrayValuedMap[Type]

  /** The type of `OrderingConstraint#lowerMap`, `OrderingConstraint#upperMap` */
  type ParamOrdering = ArrayValuedMap[List[TypeParamRef]]

  /** A new constraint with given maps */
  private def newConstraint(boundsMap: ParamBounds, lowerMap: ParamOrdering, upperMap: ParamOrdering)(using Context) : OrderingConstraint = {
    val result = new OrderingConstraint(boundsMap, lowerMap, upperMap)
    ctx.run.recordConstraintSize(result, result.boundsMap.size)
    result
  }

  /** A lens for updating a single entry array in one of the three constraint maps */
  abstract class ConstraintLens[T <: AnyRef: ClassTag] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[T]
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[T])(using Context): OrderingConstraint
    def initial: T

    def apply(c: OrderingConstraint, poly: TypeLambda, idx: Int): T = {
      val es = entries(c, poly)
      if (es == null) initial else es(idx)
    }

    /** The `current` constraint but with the entry for `param` updated to `entry`.
     *  `current` is used linearly. If it is different from `prev` it is
     *  known to be dead after the call. Hence it is OK to update destructively
     *  parts of `current` which are not shared by `prev`.
     */
    def update(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, entry: T)(using Context): OrderingConstraint = {
      var es = entries(current, poly)
      if (es != null && (es(idx) eq entry)) current
      else {
        val result =
          if (es == null) {
            es = Array.fill(poly.paramNames.length)(initial)
            updateEntries(current, poly, es)
          }
          else if (es ne entries(prev, poly))
            current // can re-use existing entries array.
          else {
            es = es.clone
            updateEntries(current, poly, es)
          }
        es(idx) = entry
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

  val boundsLens: ConstraintLens[Type] = new ConstraintLens[Type] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[Type] =
      c.boundsMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[Type])(using Context): OrderingConstraint =
      newConstraint(c.boundsMap.updated(poly, entries), c.lowerMap, c.upperMap)
    def initial = NoType
  }

  val lowerLens: ConstraintLens[List[TypeParamRef]] = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] =
      c.lowerMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(using Context): OrderingConstraint =
      newConstraint(c.boundsMap, c.lowerMap.updated(poly, entries), c.upperMap)
    def initial = Nil
  }

  val upperLens: ConstraintLens[List[TypeParamRef]] = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] =
      c.upperMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(using Context): OrderingConstraint =
      newConstraint(c.boundsMap, c.lowerMap, c.upperMap.updated(poly, entries))
    def initial = Nil
  }

  @sharable
  val empty = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty)
}

import OrderingConstraint._

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
 */
class OrderingConstraint(private val boundsMap: ParamBounds,
                         private val lowerMap : ParamOrdering,
                         private val upperMap : ParamOrdering) extends Constraint {

  type This = OrderingConstraint

// ----------- Basic indices --------------------------------------------------

  /** The number of type parameters in the given entry array */
  private def paramCount(entries: Array[Type]) = entries.length >> 1

  /** The type variable corresponding to parameter numbered `n`, null if none was created */
  private def typeVar(entries: Array[Type], n: Int): Type =
    entries(paramCount(entries) + n)

  /** The `boundsMap` entry corresponding to `param` */
  def entry(param: TypeParamRef): Type = {
    val entries = boundsMap(param.binder)
    if (entries == null) NoType
    else entries(param.paramNum)
  }

// ----------- Contains tests --------------------------------------------------

  def contains(pt: TypeLambda): Boolean = boundsMap(pt) != null

  def contains(param: TypeParamRef): Boolean = {
    val entries = boundsMap(param.binder)
    entries != null && isBounds(entries(param.paramNum))
  }

  def contains(tvar: TypeVar): Boolean = {
    val origin = tvar.origin
    val entries = boundsMap(origin.binder)
    val pnum = origin.paramNum
    entries != null && isBounds(entries(pnum)) && (typeVar(entries, pnum) eq tvar)
  }

// ---------- Dependency handling ----------------------------------------------

  def lower(param: TypeParamRef): List[TypeParamRef] = lowerLens(this, param.binder, param.paramNum)
  def upper(param: TypeParamRef): List[TypeParamRef] = upperLens(this, param.binder, param.paramNum)

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

// ---------- Info related to TypeParamRefs -------------------------------------------

  def isLess(param1: TypeParamRef, param2: TypeParamRef): Boolean =
    upper(param1).contains(param2)

  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    entry(param).bounds

  def typeVarOfParam(param: TypeParamRef): Type = {
    val entries = boundsMap(param.binder)
    if (entries == null) NoType
    else {
      val tvar = typeVar(entries, param.paramNum)
      if (tvar != null) tvar else NoType
    }
  }

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
  private def stripParams(
      tp: Type,
      todos: mutable.ListBuffer[(OrderingConstraint, TypeParamRef) => OrderingConstraint],
      isUpper: Boolean)(using Context): Type = tp match {
    case param: TypeParamRef if contains(param) =>
      todos += (if isUpper then order(_, _, param) else order(_, param, _))
      NoType
    case tp: TypeBounds =>
      val lo1 = stripParams(tp.lo, todos, !isUpper).orElse(defn.NothingType)
      val hi1 = stripParams(tp.hi, todos, isUpper).orElse(defn.AnyKindType)
      tp.derivedTypeBounds(lo1, hi1)
    case tp: AndType if isUpper =>
      val tp1 = stripParams(tp.tp1, todos, isUpper)
      val tp2 = stripParams(tp.tp2, todos, isUpper)
      if (tp1.exists)
        if (tp2.exists) tp.derivedAndType(tp1, tp2)
        else tp1
      else tp2
    case tp: OrType if !isUpper =>
      val tp1 = stripParams(tp.tp1, todos, isUpper)
      val tp2 = stripParams(tp.tp2, todos, isUpper)
      if (tp1.exists)
        if (tp2.exists) tp.derivedOrType(tp1, tp2)
        else tp1
      else tp2
    case _ =>
      tp
  }

  def add(poly: TypeLambda, tvars: List[TypeVar])(using Context): This = {
    assert(!contains(poly))
    val nparams = poly.paramNames.length
    val entries1 = new Array[Type](nparams * 2)
    poly.paramInfos.copyToArray(entries1, 0)
    tvars.copyToArray(entries1, nparams)
    newConstraint(boundsMap.updated(poly, entries1), lowerMap, upperMap).init(poly)
  }

  /** Split dependent parameters off the bounds for parameters in `poly`.
   *  Update all bounds to be normalized and update ordering to account for
   *  dependent parameters.
   */
  private def init(poly: TypeLambda)(using Context): This = {
    var current = this
    val todos = new mutable.ListBuffer[(OrderingConstraint, TypeParamRef) => OrderingConstraint]
    var i = 0
    val dropWildcards = AvoidWildcardsMap()
    while (i < poly.paramNames.length) {
      val param = poly.paramRefs(i)
      val bounds = dropWildcards(nonParamBounds(param))
      val stripped = stripParams(bounds, todos, isUpper = true)
      current = updateEntry(current, param, stripped)
      while todos.nonEmpty do
        current = todos.head(current, param)
        todos.dropInPlace(1)
      i += 1
    }
    current.checkNonCyclic()
  }

// ---------- Updates ------------------------------------------------------------

  /** If `inst` is a TypeBounds, make sure it does not contain toplevel
   *  references to `param` (see `Constraint#occursAtToplevel` for a definition
   *  of "toplevel").
   *  Any such references are replaced by `Nothing` in the lower bound and `Any`
   *  in the upper bound.
   *  References can be direct or indirect through instantiations of other
   *  parameters in the constraint.
   */
  private def ensureNonCyclic(param: TypeParamRef, inst: Type)(using Context): Type =

    def recur(tp: Type, fromBelow: Boolean): Type = tp match
      case tp: AndOrType =>
        val r1 = recur(tp.tp1, fromBelow)
        val r2 = recur(tp.tp2, fromBelow)
        if (r1 eq tp.tp1) && (r2 eq tp.tp2) then tp
        else tp.match
          case tp: OrType =>
            TypeComparer.lub(r1, r2, isSoft = tp.isSoft)
          case _ =>
            r1 & r2
      case tp: TypeParamRef =>
        if tp eq param then
          if fromBelow then defn.NothingType else defn.AnyType
        else entry(tp) match
          case NoType => tp
          case TypeBounds(lo, hi) => if lo eq hi then recur(lo, fromBelow) else tp
          case inst => recur(inst, fromBelow)
      case tp: TypeVar =>
        val underlying1 = recur(tp.underlying, fromBelow)
        if underlying1 ne tp.underlying then underlying1 else tp
      case tp: AnnotatedType =>
        val parent1 = recur(tp.parent, fromBelow)
        if parent1 ne tp.parent then tp.derivedAnnotatedType(parent1, tp.annot) else tp
      case tp: CapturingType =>
        val parent1 = recur(tp.parent, fromBelow)
        if parent1 ne tp.parent then tp.derivedCapturingType(parent1, tp.refs) else tp
      case _ =>
        val tp1 = tp.dealiasKeepAnnots
        if tp1 ne tp then
          val tp2 = recur(tp1, fromBelow)
          if tp2 ne tp1 then tp2 else tp
        else tp

    inst match
      case bounds: TypeBounds =>
        bounds.derivedTypeBounds(
          recur(bounds.lo, fromBelow = true),
          recur(bounds.hi, fromBelow = false))
      case _ =>
        inst
  end ensureNonCyclic

  /** Add the fact `param1 <: param2` to the constraint `current` and propagate
   *  `<:<` relationships between parameters ("edges") but not bounds.
   */
  private def order(current: This, param1: TypeParamRef, param2: TypeParamRef)(using Context): This =
    if (param1 == param2 || current.isLess(param1, param2)) this
    else {
      assert(contains(param1), i"$param1")
      assert(contains(param2), i"$param2")
      val newUpper = param2 :: exclusiveUpper(param2, param1)
      val newLower = param1 :: exclusiveLower(param1, param2)
      val current1 = newLower.foldLeft(current)(upperLens.map(this, _, _, newUpper ::: _))
      val current2 = newUpper.foldLeft(current1)(lowerLens.map(this, _, _, newLower ::: _))
      current2
    }

  /** The list of parameters P such that, for a fresh type parameter Q:
   *
   *    Q <: tp  implies  Q <: P      and isUpper = true, or
   *    tp <: Q  implies  P <: Q      and isUpper = false
   */
  private def dependentParams(tp: Type, isUpper: Boolean)(using Context): List[TypeParamRef] = tp match
    case param: TypeParamRef if contains(param) =>
      param :: (if (isUpper) upper(param) else lower(param))
    case tp: AndType if isUpper  =>
      dependentParams(tp.tp1, isUpper) | (dependentParams(tp.tp2, isUpper))
    case tp: OrType if !isUpper =>
      dependentParams(tp.tp1, isUpper).intersect(dependentParams(tp.tp2, isUpper))
    case EtaExpansion(tycon) =>
      dependentParams(tycon, isUpper)
    case _ =>
      Nil

  private def updateEntry(current: This, param: TypeParamRef, tp: Type)(using Context): This = {
    if Config.checkNoWildcardsInConstraint then assert(!tp.containsWildcardTypes)
    var current1 = boundsLens.update(this, current, param, tp)
    tp match {
      case TypeBounds(lo, hi) =>
        for p <- dependentParams(lo, isUpper = false) do
          current1 = order(current1, p, param)
        for p <- dependentParams(hi, isUpper = true) do
          current1 = order(current1, param, p)
      case _ =>
    }
    current1
  }

  /** The public version of `updateEntry`. Guarantees that there are no cycles */
  def updateEntry(param: TypeParamRef, tp: Type)(using Context): This =
    updateEntry(this, param, ensureNonCyclic(param, tp)).checkNonCyclic()

  def addLess(param1: TypeParamRef, param2: TypeParamRef)(using Context): This =
    order(this, param1, param2).checkNonCyclic()

  def unify(p1: TypeParamRef, p2: TypeParamRef)(using Context): This =
    val bound1 = nonParamBounds(p1).substParam(p2, p1)
    val bound2 = nonParamBounds(p2).substParam(p2, p1)
    val p1Bounds = bound1 & bound2
    updateEntry(p1, p1Bounds).replace(p2, p1)

// ---------- Replacements and Removals -------------------------------------

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   */
  def replace(param: TypeParamRef, tp: Type)(using Context): OrderingConstraint =
    val replacement = tp.dealiasKeepAnnots.stripTypeVar
    if param == replacement then this.checkNonCyclic()
    else
      assert(replacement.isValueTypeOrLambda)
      var current =
        if isRemovable(param.binder) then remove(param.binder)
        else updateEntry(this, param, replacement)

      def removeParam(ps: List[TypeParamRef]) = ps.filterConserve(param ne _)

      def replaceParam(tp: Type, atPoly: TypeLambda, atIdx: Int): Type =
        current.ensureNonCyclic(atPoly.paramRefs(atIdx), tp.substParam(param, replacement))

      current.foreachParam { (p, i) =>
        current = boundsLens.map(this, current, p, i, replaceParam(_, p, i))
        current = lowerLens.map(this, current, p, i, removeParam)
        current = upperLens.map(this, current, p, i, removeParam)
      }
      current.checkNonCyclic()
  end replace

  def remove(pt: TypeLambda)(using Context): This = {
    def removeFromOrdering(po: ParamOrdering) = {
      def removeFromBoundss(key: TypeLambda, bndss: Array[List[TypeParamRef]]): Array[List[TypeParamRef]] = {
        val bndss1 = bndss.map(_.filterConserve(_.binder ne pt))
        if (bndss.corresponds(bndss1)(_ eq _)) bndss else bndss1
      }
      po.remove(pt).mapValuesNow(removeFromBoundss)
    }
    newConstraint(boundsMap.remove(pt), removeFromOrdering(lowerMap), removeFromOrdering(upperMap))
      .checkNonCyclic()
  }

  def isRemovable(pt: TypeLambda): Boolean = {
    val entries = boundsMap(pt)
    @tailrec def allRemovable(last: Int): Boolean =
      if (last < 0) true
      else typeVar(entries, last) match {
        case tv: TypeVar => tv.inst.exists && allRemovable(last - 1)
        case _ => false
      }
    allRemovable(paramCount(entries) - 1)
  }

// ----------- Joins -----------------------------------------------------

  def & (other: Constraint, otherHasErrors: Boolean)(using Context): OrderingConstraint = {

    def merge[T](m1: ArrayValuedMap[T], m2: ArrayValuedMap[T], join: (T, T) => T): ArrayValuedMap[T] = {
      var merged = m1
      def mergeArrays(xs1: Array[T], xs2: Array[T]) = {
        val xs = xs1.clone
        for (i <- xs.indices) xs(i) = join(xs1(i), xs2(i))
        xs
      }
      m2.foreachBinding { (poly, xs2) =>
        merged = merged.updated(poly,
            if (m1.contains(poly)) mergeArrays(m1(poly), xs2) else xs2)
      }
      merged
    }

    def mergeParams(ps1: List[TypeParamRef], ps2: List[TypeParamRef]) =
      ps2.foldLeft(ps1)((ps1, p2) => if (ps1.contains(p2)) ps1 else p2 :: ps1)

    // Must be symmetric
    def mergeEntries(e1: Type, e2: Type): Type =
      (e1, e2) match {
        case _ if e1 eq e2 => e1
        case (e1: TypeBounds, e2: TypeBounds) => e1 & e2
        case (e1: TypeBounds, _) if e1 contains e2 => e2
        case (_, e2: TypeBounds) if e2 contains e1 => e1
        case (tv1: TypeVar, tv2: TypeVar) if tv1 eq tv2 => e1
        case _ =>
          if (otherHasErrors)
            e1
          else
            throw new AssertionError(i"cannot merge $this with $other, mergeEntries($e1, $e2) failed")
      }

    val that = other.asInstanceOf[OrderingConstraint]

    new OrderingConstraint(
        merge(this.boundsMap, that.boundsMap, mergeEntries),
        merge(this.lowerMap, that.lowerMap, mergeParams),
        merge(this.upperMap, that.upperMap, mergeParams))
  }.showing(i"constraint merge $this with $other = $result", constr)

  def hasConflictingTypeVarsFor(tl: TypeLambda, that: Constraint): Boolean =
    contains(tl) && that.contains(tl) &&
    // Since TypeVars are allocated in bulk for each type lambda, we only have
    // to check the first one to find out if some of them are different.
    (this.typeVarOfParam(tl.paramRefs(0)) ne that.typeVarOfParam(tl.paramRefs(0)))

  def subst(from: TypeLambda, to: TypeLambda)(using Context): OrderingConstraint =
    def swapKey[T](m: ArrayValuedMap[T]) = m.remove(from).updated(to, m(from))
    var current = newConstraint(swapKey(boundsMap), swapKey(lowerMap), swapKey(upperMap))
    def subst[T <: Type](x: T): T = x.subst(from, to).asInstanceOf[T]
    current.foreachParam {(p, i) =>
      current = boundsLens.map(this, current, p, i, subst)
      current = lowerLens.map(this, current, p, i, _.map(subst))
      current = upperLens.map(this, current, p, i, _.map(subst))
    }
    constr.println(i"renamed $this to $current")
    current.checkNonCyclic()

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
        val TypeBounds(lo, hi) :: pinfos1 = tl.paramInfos
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
          case tv: TypeVar if !tv.inst.exists => op(tv)
          case _ =>
        i += 1
    }

  private var myUninstVars: mutable.ArrayBuffer[TypeVar] = _

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar] = {
    if (myUninstVars == null || myUninstVars.exists(_.inst.exists)) {
      myUninstVars = new mutable.ArrayBuffer[TypeVar]
      boundsMap.foreachBinding { (poly, entries) =>
        for (i <- 0 until paramCount(entries))
          typeVar(entries, i) match {
            case tv: TypeVar if !tv.inst.exists && isBounds(entries(i)) => myUninstVars += tv
            case _ =>
          }
      }
    }
    myUninstVars
  }

// ---------- Checking -----------------------------------------------

  def checkNonCyclic()(using Context): this.type =
    if Config.checkConstraintsNonCyclic then
      domainParams.foreach { param =>
        val inst = entry(param)
        assert(!isLess(param, param),
          s"cyclic ordering involving $param in ${this.show}, upper = $inst")
        assert(!occursAtToplevel(param, inst),
          s"cyclic bound for $param: ${inst.show} in ${this.show}")
      }
    this

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

  override def checkClosed()(using Context): Unit =

    def isFreeTypeParamRef(tp: Type) = tp match
      case TypeParamRef(binder: TypeLambda, _) => !contains(binder)
      case _ => false

    def checkClosedType(tp: Type, where: String) =
      if tp != null then
        assert(!tp.existsPart(isFreeTypeParamRef), i"unclosed constraint: $this refers to $tp in $where")

    boundsMap.foreachBinding((_, tps) => tps.foreach(checkClosedType(_, "bounds")))
    lowerMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "lower"))))
    upperMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "upper"))))
  end checkClosed

// ---------- toText -----------------------------------------------------

  private def contentsToText(printer: Printer): Text =
    //Printer.debugPrintUnique = true
    def entryText(tp: Type) = tp match {
      case tp: TypeBounds =>
        tp.toText(printer)
      case _ =>
        " := " ~ tp.toText(printer)
    }
    val indent = 3
    val uninstVarsText = " uninstantiated variables: " ~
      Text(uninstVars.map(_.toText(printer)), ", ")
    val constrainedText =
      " constrained types: " ~ Text(domainLambdas map (_.toText(printer)), ", ")
    val boundsText =
      " bounds: " ~ {
        val assocs =
          for (param <- domainParams)
          yield (" " * indent) ~ param.toText(printer) ~ entryText(entry(param))
        Text(assocs, "\n")
      }
    val orderingText =
      " ordering: " ~ {
        val deps =
          for {
            param <- domainParams
            ups = minUpper(param)
            if ups.nonEmpty
          }
          yield
            (" " * indent) ~ param.toText(printer) ~ " <: " ~
              Text(ups.map(_.toText(printer)), ", ")
        Text(deps, "\n")
      }
    //Printer.debugPrintUnique = false
    Text.lines(List(uninstVarsText, constrainedText, boundsText, orderingText))

  override def toText(printer: Printer): Text =
    Text.lines(List("Constraint(", contentsToText(printer), ")"))

  def contentsToString(using Context): String =
    contentsToText(ctx.printer).show

  override def toString: String = {
    def entryText(tp: Type): String = tp match {
      case tp: TypeBounds => tp.toString
      case _ => " := " + tp
    }
    val constrainedText =
      " constrained types = " + domainLambdas.mkString("\n")
    val boundsText = domainLambdas
      " bounds = " + {
        val assocs =
          for (param <- domainParams)
          yield
            s"${param.binder.paramNames(param.paramNum)}: ${entryText(entry(param))}"
        assocs.mkString("\n")
    }
    constrainedText + "\n" + boundsText
  }
}
