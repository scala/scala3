package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Decorators._
import util.SimpleMap
import collection.mutable
import printing.{Printer, Showable}
import printing.Texts._
import config.Config
import config.Printers._
import collection.immutable.BitSet
import reflect.ClassTag

object TrackingConstraint {

  /** The type of `Constraint#myMap` */
  type ParamInfo = SimpleMap[PolyType, Array[Type]]

}

import TrackingConstraint._

/** Constraint over undetermined type parameters
 *  @param myMap a map from PolyType to arrays.
 *               Each array contains twice the number of entries as there a type parameters
 *               in the PolyType. The first half of the array contains the type bounds that constrain the
 *               polytype's type parameters. The second half might contain type variables that
 *               track the corresponding parameters, or is left empty (filled with nulls).
 *               An instantiated type parameter is represented by having its instance type in
 *               the corresponding array entry.
 */
class TrackingConstraint(private val myMap: ParamInfo, 
                         private val less: Array[BitSet], 
                         private val params: Array[PolyParam]) extends Constraint {
  
  type This = TrackingConstraint
  
  assert(less.length == params.length)
  
  /** A new constraint which is derived from this constraint by adding or replacing
   *  the entries corresponding to `pt` with `entries`.
   */
  private def newConstraint(myMap: ParamInfo, less: Array[BitSet], params: Array[PolyParam])(implicit ctx: Context) : TrackingConstraint = {
    val result = new TrackingConstraint(myMap, less, params)
    if (Config.checkConstraintsNonCyclic) result.checkNonCyclic()
    ctx.runInfo.recordConstraintSize(result, result.myMap.size)
    result
  }
  
// ----------- Basic indices --------------------------------------------------

  /** The immutable array of constrained polytypes */
  private val polyTypes = new Array[PolyType](myMap.size)
  
  /** The start positions of parameters of constrained polytypes in `params` and `less` */
  private val polyStart = new Array[Int](myMap.size)
  
  {
    var idx = 0
    var count = 0
    myMap.foreachBinding { (pt, _) => 
      polyTypes(idx) = pt
      polyStart(idx) = count
      count += pt.paramNames.length
      idx += 1
    }
    assert(count == params.length)
  }
  
  /** The index of given polytype `pt` in this constraint, 
   *  or `polyTypes.length` if constraint does not contain `pt`.
   */
  private def polyIndex(pt: PolyType): Int = {
    var i = 0
    while (i < polyTypes.length && (polyTypes(i) ne pt)) i += 1
    i
  }
  
  /** The index of the first parameter of given polytype `pt` in this constraint */
  private def polyStart(pt: PolyType): Int = this.polyStart.apply(polyIndex(pt))
  
  /** The index of `param` in `params` and `less` */
  private def paramIndex(param: PolyParam): Int = {
    assert(contains(param.binder))
    polyStart(param.binder) + param.paramNum
  }
  
  /** The number of type parameters in the given entry array */
  private def paramCount(entries: Array[Type]) = entries.length >> 1

  /** The type variable corresponding to parameter numbered `n`, null if none was created */
  private def typeVar(entries: Array[Type], n: Int): Type =
    entries(paramCount(entries) + n)

  private def entry(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType
    else entries(param.paramNum)
  }
    
// ----------- Contains tests --------------------------------------------------

  def contains(pt: PolyType): Boolean = polyIndex(pt) < polyTypes.length

  def contains(param: PolyParam): Boolean = {
    val entries = myMap(param.binder)
    entries != null && entries(param.paramNum).isInstanceOf[TypeBounds]
  }

  def contains(tvar: TypeVar): Boolean = {
    val origin = tvar.origin
    val entries = myMap(origin.binder)
    val pnum = origin.paramNum
    entries != null && isBounds(entries(pnum)) && (typeVar(entries, pnum) eq tvar)
  }

  private def isBounds(tp: Type) = tp.isInstanceOf[TypeBounds]
  
// ---------- Dependency handling ----------------------------------------------
  
  private def upperBits(i: Int): BitSet = less(i)
  
  private def lowerBits(i: Int): BitSet = 
    (BitSet() /: less.indices) ((bits, j) => if (less(i)(j)) bits + j else bits)
    
  private def minUpperBits(i: Int): BitSet = {
    val all = upperBits(i)
    all.filterNot(j => all.exists(k => less(k)(j)))
  }
  
  private def minLowerBits(i: Int): BitSet = {
    val all = lowerBits(i)
    all.filterNot(j => all.exists(k => less(j)(k)))
  }
  
  private def overParams(op: Int => BitSet): PolyParam => List[PolyParam] = param =>
    op(paramIndex(param)).toList.map(params).filter(contains)
    
  val upper    = overParams(upperBits)
  val lower    = overParams(lowerBits)
  val minUpper = overParams(minUpperBits)
  val minLower = overParams(minLowerBits)
  
  
// ---------- Info related to PolyParams -------------------------------------------

  def related(param1: PolyParam, param2: PolyParam, firstIsLower: Boolean)(implicit ctx: Context): Boolean = {
    val i1 = paramIndex(param1)
    val i2 = paramIndex(param2)
    if (firstIsLower) less(i1)(i2) else less(i2)(i1)
  }

  def nonParamBounds(param: PolyParam)(implicit ctx: Context): TypeBounds =
    entry(param).asInstanceOf[TypeBounds]
  
  def bounds(param: PolyParam)(implicit ctx: Context): TypeBounds = {
    val bounds @ TypeBounds(lo, hi) = nonParamBounds(param)
    bounds.derivedTypeBounds(
        (lo /: minLower(param))(OrType.apply), 
        (hi /: minUpper(param))(AndType.apply))
  }
  
  def at(param: PolyParam)(implicit ctx: Context): Type = {
    entry(param) match {
      case _: TypeBounds => bounds(param)
      case e => e
    }
  }
  
  def typeVarOfParam(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType
    else {
      val tvar = typeVar(entries, param.paramNum)
      if (tvar != null) tvar else NoType
    }
  }
  
// ---------- Type splitting --------------------------------------------------

  /** The set of "dependent" constrained parameters that unconditionally strengthen bound `tp`.
   *  @param seenFromBelow   If true, `bound` is an upper bound, else a lower bound.
   */
  private def depParams(tp: Type, seenFromBelow: Boolean): Set[PolyParam] = tp match {
    case tp: PolyParam if contains(tp) => 
      Set(tp)
    case tp: AndOrType if seenFromBelow == tp.isAnd =>
      depParams(tp.tp1, seenFromBelow) | depParams(tp.tp2, seenFromBelow)
    case _ =>
      Set.empty
  }
   
  /** The bound type `tp` without dependent parameters.
   *  NoType if type consists only of dependent parameters.
   *  @param seenFromBelow   If true, `bound` is an upper bound, else a lower bound.
   */
  private def stripParams(tp: Type, seenFromBelow: Boolean)(implicit ctx: Context): Type = tp match {
    case tp: PolyParam if contains(tp) => 
      NoType
    case tp: AndOrType if seenFromBelow == tp.isAnd =>
      val tp1 = nonParamType(tp.tp1, seenFromBelow)
      val tp2 = nonParamType(tp.tp2, seenFromBelow)
      if (tp1.exists)
        if (tp2.exists) tp.derivedAndOrType(tp1, tp2)
        else tp1
      else tp2
    case _ =>
      tp
  } 
  
  /** The bound type `tp` without dependent parameters.
   *  A top or bottom type if type consists only of dependent parameters.
   *  @param seenFromBelow   If true, `bound` is an upper bound, else a lower bound.
   */
  private def nonParamType(tp: Type, seenFromBelow: Boolean)(implicit ctx: Context): Type = 
    stripParams(tp, seenFromBelow).orElse(if (seenFromBelow) defn.AnyType else defn.NothingType)
  
  /** The `tp1 is a TypeBounds type, the bounds without dependent parameters,
   *  otherwise `tp`.
   */
  private def nonParamType(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp @ TypeBounds(lo, hi) =>
      tp.derivedTypeBounds(
        nonParamType(lo, seenFromBelow = false),
        nonParamType(hi, seenFromBelow = true))
    case _ =>
      tp
  }

  /** An updated partial order matrix that incorporates `less` and also reflects the new `bounds`
   *  for parameter `param`. 
   */
  private def updatedLess(less: Array[BitSet], param: PolyParam, bounds: Type): Array[BitSet] = bounds match {
    case TypeBounds(lo, hi) =>
      updatedLess(
          updatedLess(less, param, lo, seenFromBelow = false), 
          param, hi, seenFromBelow = true)
    case _ =>
      less
  }
  
  /** An updated partial order matrix that incorporates `less` and also reflects that `param` has a new
   *  `bound`, where `seenFromBelow` is true iff `bound` is an upper bound for `param`.
   */
  def updatedLess(less: Array[BitSet], param: PolyParam, bound: Type, seenFromBelow: Boolean): Array[BitSet] = 
    updatedLess(less, param, depParams(bound, seenFromBelow).iterator, inOrder = seenFromBelow)
    
  /** An updated partial order matrix that incorporates `less` and also reflects that `param` relates
   *  to all parameters in `ps2` wrt <:< if `inOrder` is true, `>:>` otherwise.
   */
  def updatedLess(less: Array[BitSet], p1: PolyParam, ps2: Iterator[PolyParam], inOrder: Boolean): Array[BitSet] = 
    if (ps2.hasNext) updatedLess(updatedLess(less, p1, ps2.next, inOrder), p1, ps2, inOrder)
    else less
  
  /** An updated partial order matrix that incorporates `less` and also reflects that `param` relates
   *  to `p2` wrt <:< if `inOrder` is true, `>:>` otherwise.
   */
  def updatedLess(less: Array[BitSet], p1: PolyParam, p2: PolyParam, inOrder: Boolean): Array[BitSet] =
    if (!inOrder) updatedLess(less, p2, p1, true)
    else {
      val i1 = paramIndex(p1)
      val i2 = paramIndex(p2)
      if (i1 == i2 || less(i1)(i2)) less
      else {
        val result = less.clone
        result(i1) = result(i1) + i2 | upperBits(i2)
        assert(!result(i1)(i1))
        for (j <- lowerBits(i1)) {
          result(j) = result(j) + i2 | upperBits(i2)
          assert(!result(j)(j))
        }
        result
      }
    } 
  
// ---------- Updates ------------------------------------------------------------

  def order(param: PolyParam, bound: PolyParam, inOrder: Boolean)(implicit ctx: Context): This = {
    val less1 = updatedLess(less, param, bound, inOrder)
    if (less1 eq less) this else newConstraint(myMap, less1, params)
  }
  
  def nonParamUpdated(param: PolyParam, tpe: Type)(implicit ctx: Context): This = {
    val entries1 = myMap(param.binder).clone
    entries1(param.paramNum) = tpe
    newConstraint(myMap.updated(param.binder, entries1), less, params)
  }

  def updated(param: PolyParam, tpe: Type)(implicit ctx: Context): This = {
    val less1 = updatedLess(less, param, tpe)
    val entries = myMap(param.binder)
    val entry1 = nonParamType(tpe)
    val idx = param.paramNum
    val entries1 =
      if (entry1 eq entries(idx)) entries
      else {
        val entries1 = entries.clone
        entries1(idx) = entry1
        entries1
      }
    newConstraint(myMap.updated(param.binder, entries1), less1, params)
  }
  
  /** Drop parameter `PolyParam(poly, n)` from `bounds`,
   *  replacing with Nothing in the lower bound and by `Any` in the upper bound.
   */
  private def dropParamIn(bounds: TypeBounds, poly: PolyType, n: Int)(implicit ctx: Context): TypeBounds = {
    def drop(tp: Type): Type = tp match {
      case tp: AndOrType =>
        val tp1 = drop(tp.tp1)
        val tp2 = drop(tp.tp2)
        if (!tp1.exists) tp2
        else if (!tp2.exists) tp1
        else tp
      case PolyParam(`poly`, `n`) => NoType
      case _ => tp
    }
    def approx(tp: Type, limit: Type): Type = {
      val tp1 = drop(tp)
      if (tp1.exists || !tp.exists) tp1 else limit
    }
    bounds.derivedTypeBounds(
        approx(bounds.lo, defn.NothingType), approx(bounds.hi, defn.AnyType))
  }

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   */
  def replace(param: PolyParam, tp: Type)(implicit ctx: Context): TrackingConstraint = {
    val replacement = tp.dealias.stripTypeVar

    def subst(poly: PolyType, entries: Array[Type]) = {
      var result = entries
      var i = 0
      while (i < paramCount(entries)) {
        entries(i) match {
          case oldBounds: TypeBounds =>
            val newBounds = oldBounds.substParam(param, replacement).asInstanceOf[TypeBounds]
            if (oldBounds ne newBounds) {
              if (result eq entries) result = entries.clone
              result(i) = dropParamIn(newBounds, poly, i)
            }
          case _ =>
        }
        i += 1
      }
      result
    }
    
    if (param == replacement) this 
    else {
      val pt = param.binder
      val constr1 = if (isRemovable(pt, param.paramNum)) remove(pt) else updated(param, replacement)
      val result = new TrackingConstraint(constr1.myMap mapValues subst, constr1.less, constr1.params)
      if (Config.checkConstraintsNonCyclic) result.checkNonCyclic()
      result
    }
  }

  def unify(p1: PolyParam, p2: PolyParam)(implicit ctx: Context): This = {
    val p1Bounds =
      dropParamIn(nonParamBounds(p1), p2.binder, p2.paramNum) &
      dropParamIn(nonParamBounds(p2), p1.binder, p1.paramNum)
    this.nonParamUpdated(p1, p1Bounds).nonParamUpdated(p2, p1)
  }

  def add(poly: PolyType, tvars: List[TypeVar])(implicit ctx: Context): This = {
    assert(!contains(poly))
    val nparams = poly.paramNames.length
    val entries1 = new Array[Type](nparams * 2)
    poly.paramBounds.copyToArray(entries1, 0)
    tvars.copyToArray(entries1, nparams)
    val is = poly.paramBounds.indices
    val newParams = is.map(PolyParam(poly, _))
    val params1 = params ++ newParams
    var less1 = less ++ is.map(Function.const(BitSet.empty))
    for (i <- is) {
      less1 = updatedLess(less1, newParams(i), entries1(i))
      entries1(i) = nonParamType(entries1(i))
    }
    newConstraint(myMap.updated(poly, entries1), less1, params1)
  }

  /** A new constraint with all entries coming from `pt` removed. */
  def remove(pt: PolyType)(implicit ctx: Context): This = {
    val start = polyStart(pt)
    val skipped = pt.paramNames.length
    
    def shrinkSet(bits: BitSet): BitSet =
      (BitSet() /: bits) ((res, i) =>
        if (i < start) res + i
        else if (i < start + skipped) res
        else res + (i - skipped))
    def shrinkArray[T: ClassTag](src: Array[T]) = {
      val dst = new Array[T](src.length - skipped)
      Array.copy(src, 0, dst, 0, start)
      Array.copy(src, start + skipped, dst, start, dst.length - start) 
      dst
    }
    newConstraint(
      myMap = myMap remove pt, 
      less = shrinkArray(less).map(shrinkSet(_)), 
      params = shrinkArray(params))
  }

  def isRemovable(pt: PolyType, removedParam: Int = -1): Boolean = {
    val entries = myMap(pt)
    var noneLeft = true
    var i = paramCount(entries)
    while (noneLeft && i > 0) {
      i -= 1
      if (i != removedParam && isBounds(entries(i))) noneLeft = false
      else typeVar(entries, i) match {
        case tv: TypeVar =>
          if (!tv.inst.exists) noneLeft = false // need to keep line around to compute instType
        case _ =>
      }
    }
    noneLeft
  }

// ---------- Exploration --------------------------------------------------------

  def domainPolys: List[PolyType] = polyTypes.toList

  def domainParams: List[PolyParam] = params.toList

  def forallParams(p: PolyParam => Boolean): Boolean = {
    myMap.foreachBinding { (poly, entries) =>
      for (i <- 0 until paramCount(entries))
        if (isBounds(entries(i)) && !p(PolyParam(poly, i))) return false
    }
    true
  }

  def foreachTypeVar(op: TypeVar => Unit): Unit =
    myMap.foreachBinding { (poly, entries) =>
      for (i <- 0 until paramCount(entries)) {
        typeVar(entries, i) match {
          case tv: TypeVar if !tv.inst.exists => op(tv)
          case _ =>
        }
      }
    }

  private var myUninstVars: mutable.ArrayBuffer[TypeVar] = _

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar] = {
    if (myUninstVars == null) {
      myUninstVars = new mutable.ArrayBuffer[TypeVar]
      myMap.foreachBinding { (poly, entries) =>
        for (i <- 0 until paramCount(entries)) {
          typeVar(entries, i) match {
            case tv: TypeVar if isBounds(entries(i)) => myUninstVars += tv
            case _ =>
          }
        }
      }
    }
    myUninstVars
  }

// ---------- Cyclic checking -------------------------------------------

  private def checkNonCyclic(idx: Int)(implicit ctx: Context): Unit =
    assert(!less(idx)(idx), i"cyclic constraint involving ${params(idx)}")

  def checkNonCyclic(pt: PolyType, entries: Array[Type])(implicit ctx: Context): Unit =
    for (i <- entries.indices) checkNonCyclic(paramIndex(PolyParam(pt, i)))

  def checkNonCyclic()(implicit ctx: Context): Unit =
    for (i <- params.indices) checkNonCyclic(i)

  def checkNonCyclicTrans()(implicit ctx: Context): Unit = checkNonCyclic()
  
// ---------- toText -----------------------------------------------------

  override def toText(printer: Printer): Text = {
    def entryText(tp: Type) = tp match {
      case tp: TypeBounds => 
        tp.toText(printer)
      case _ => 
        " := " ~ tp.toText(printer)
    }
    val indent = 3
    val header: Text = "Constraint("
    val uninstVarsText = " uninstVars = " ~
      Text(uninstVars map (_.toText(printer)), ", ") ~ ";"
    val constrainedText =
      " constrained types = " ~ Text(domainPolys map (_.toText(printer)), ", ")
    val boundsText =
      " bounds = " ~ {
        val assocs =
          for (param <- domainParams)
          yield (" " * indent) ~ param.toText(printer) ~ entryText(entry(param))
        Text(assocs, "\n")
      }
    val orderingText =
      " ordering = " ~ {
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
    Text.lines(List(header, uninstVarsText, constrainedText, boundsText, orderingText, ")"))
  }
}

