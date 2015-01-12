package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._
import util.SimpleMap
import collection.mutable
import printing.{Printer, Showable}
import printing.Texts._
import config.Config
import config.Printers._

object NaiveConstraint {

  /** The type of `Constraint#myMap` */
  type ParamInfo = SimpleMap[PolyType, Array[Type]]

  /** The type of `Constraint#dependents */
  type DependentMap = SimpleMap[PolyType, Array[Set[PolyParam]]]

  /** The type of functions that include or exclude a `PolyParam` in or from a set*/
  private type DepDelta = (Set[PolyParam], PolyParam) => Set[PolyParam]

  private val addDep: DepDelta = (_ + _)
  private val removeDep: DepDelta = (_ - _)

  private val NoTypeBounds = new TypeBounds(WildcardType, WildcardType) {
    override def computeHash = unsupported("computeHash")
  }

  /** An accumulator that changes dependencies on `param`.
   *  @param  param       The parameter to which changed dependencies refer.
   *  @param  ofVariance  Include `PolyParams` occurring at this variance in the dependencies.
   *  @param  delta       The dependency change to perform (add or remove).
   */
  private class ChangeDependencies(param: PolyParam, ofVariance: Int, delta: DepDelta)(implicit ctx: Context)
  extends TypeAccumulator[DependentMap] {
    def apply(deps: DependentMap, tp: Type): DependentMap = tp match {
      case tp @ PolyParam(pt, n) if
        this.variance == 0 || this.variance == ofVariance =>
        val oldDeps = deps(pt)
        val original = safeSelect(oldDeps, n)
        val changed = delta(original, param)
        if (original eq changed) deps
        else {
          val newDeps =
            if (oldDeps == null) new Array[Set[PolyParam]](pt.paramBounds.length)
            else oldDeps.clone
          newDeps(n) = changed
          deps.updated(pt, newDeps)
        }
      case _ => foldOver(deps, tp)
    }
  }

  /** `deps(n)`, except that `Set()` is returned if `deps` or `deps(n)` are null */
  private def safeSelect(deps: Array[Set[PolyParam]], n: Int) : Set[PolyParam] =
    if (deps == null || deps(n) == null) Set()
    else deps(n)
    
  private def ignoreParam(p: PolyParam): Unit = {}
}

import NaiveConstraint._

/** Constraint over undetermined type parameters
 *  @param myMap a map from PolyType to arrays.
 *               Each array contains twice the number of entries as there a type parameters
 *               in the PolyType. The first half of the array contains the type bounds that constrain the
 *               polytype's type parameters. The second half might contain type variables that
 *               track the corresponding parameters, or is left empty (filled with nulls).
 *               An instantiated type parameter is represented by having its instance type in
 *               the corresponding array entry.
 */
class NaiveConstraint(private val myMap: ParamInfo) extends Constraint {
  
  type This = NaiveConstraint

  def contains(pt: PolyType): Boolean = myMap(pt) != null

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

  /** The number of type parameters in the given entry array */
  private def paramCount(entries: Array[Type]) = entries.length >> 1

  /** The type variable corresponding to parameter numbered `n`, null if none was created */
  private def typeVar(entries: Array[Type], n: Int): Type =
    entries(paramCount(entries) + n)

  private def isBounds(tp: Type) = tp.isInstanceOf[TypeBounds]

  def at(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType else entries(param.paramNum)
  }

  def bounds(param: PolyParam): TypeBounds = at(param).asInstanceOf[TypeBounds]
  
  def nonParamBounds(param: PolyParam)(implicit ctx: Context): TypeBounds = {
    val bs @ TypeBounds(lo, hi) = bounds(param)
    val lo1 = splitParams(lo, seenFromBelow = false, ignoreParam)
    val hi1 = splitParams(hi, seenFromBelow = true, ignoreParam)
    bs.derivedTypeBounds(lo1.orElse(defn.NothingType), hi1.orElse(defn.AnyType))
  }
  
  def related(param1: PolyParam, param2: PolyParam, firstIsLower: Boolean)(implicit ctx: Context): Boolean = {
    var isRelated = false
    def registerParam(p: PolyParam) = if (p == param2) isRelated = true
    val TypeBounds(lo, hi) = bounds(param1)
    if (firstIsLower) splitParams(hi, seenFromBelow = true, registerParam)
    else splitParams(lo, seenFromBelow = false, registerParam)
    isRelated
  }

  def typeVarOfParam(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType
    else {
      val tvar = typeVar(entries, param.paramNum)
      if (tvar != null) tvar else NoType
    }
  }

  /** A new constraint which is derived from this constraint by adding or replacing
   *  the entries corresponding to `pt` with `entries`.
   */
  private def updateEntries(pt: PolyType, entries: Array[Type])(implicit ctx: Context) : NaiveConstraint = {
    val res = new NaiveConstraint(myMap.updated(pt, entries))
      
    //assert(res.domainPolys.filter(pt =>
    //  pt.resultType.resultType.widen.classSymbol.name.toString == "Ensuring").length < 2) //DEBUG
    if (Config.checkConstraintsNonCyclic) checkNonCyclic(pt, entries)
    ctx.runInfo.recordConstraintSize(res, res.myMap.size)
    res
  }

  def checkNonCyclic(pt: PolyType, entries: Array[Type])(implicit ctx: Context): Unit =
    for ((entry, i) <- entries.zipWithIndex) {
      val param = PolyParam(pt, i)
      entry match {
        case TypeBounds(lo, hi) =>
          assert(!param.occursIn(lo, fromBelow = true), s"$param occurs below $lo")
          assert(!param.occursIn(hi, fromBelow = false), s"$param occurs above $hi")
        case _ =>
      }
    }

  def checkNonCyclic()(implicit ctx: Context): Unit = {
    for (pt <- domainPolys) checkNonCyclic(pt, myMap(pt))
  }

  /** Check that no constrained parameter contains itself as a bound,
   *  either directly or indirectly. This should be not a structer criterion
   *  than checkNonCyclic because transitivity should be eliminated always,
   *  but it's good to be paranoid.
   */
  def checkNonCyclicTrans()(implicit ctx: Context): Unit = {
    for (pt <- domainPolys) 
      checkNonCyclicTrans(pt, myMap(pt))
  }
  
  private def checkNonCyclicTrans(pt: PolyType, entries: Array[Type])(implicit ctx: Context): Unit =
    for ((entry, i) <- entries.zipWithIndex) {
      def occursIn(params: Set[PolyParam], bound: Type, fromBelow: Boolean): Boolean = bound.stripTypeVar match {
        case bound: PolyParam => 
          params.contains(bound) || {
            at(bound) match {
              case TypeBounds(lo, hi) =>
                occursIn(params + bound, if (fromBelow) lo else hi, fromBelow)
              case _ =>
                false
            }
          }
        case bound: AndOrType =>
          def occ1 = occursIn(params, bound.tp1, fromBelow)
          def occ2 = occursIn(params, bound.tp2, fromBelow)
          if (fromBelow == bound.isAnd) occ1 && occ2 else occ1 || occ2
        case _ => false
      }
      val param = PolyParam(pt, i)
      entry match {
        case TypeBounds(lo, hi) =>
          assert(!occursIn(Set(param), lo, fromBelow = true), s"$param occurs below $lo")
          assert(!occursIn(Set(param), hi, fromBelow = false), s"$param occurs above $hi")
        case _ =>
      }
    }
  
  def updated(param: PolyParam, tpe: Type)(implicit ctx: Context): This = {
    val newEntries = myMap(param.binder).clone
    newEntries(param.paramNum) = tpe
    updateEntries(param.binder, newEntries)
  }
  
  def order(param: PolyParam, bound: PolyParam, fromBelow: Boolean)(implicit ctx: Context): This =
    if (related(param, bound, firstIsLower = !fromBelow)) this
    else {
      val oldBounds = bounds(param)
      val newBounds = 
        if (fromBelow) TypeBounds(OrType(oldBounds.lo, bound), oldBounds.hi)
        else TypeBounds(oldBounds.lo, AndType(oldBounds.hi, bound))
      updated(param, newBounds)
    }
  
  /** A new constraint with all entries coming from `pt` removed. */
  def remove(pt: PolyType)(implicit ctx: Context): This =
    new NaiveConstraint(myMap remove pt)

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
  private def uncheckedReplace(param: PolyParam, tp: Type)(implicit ctx: Context): NaiveConstraint = {

    def subst(poly: PolyType, entries: Array[Type]) = {
      var result = entries
      var i = 0
      while (i < paramCount(entries)) {
        entries(i) match {
          case oldBounds: TypeBounds =>
            val newBounds = oldBounds.substParam(param, tp).asInstanceOf[TypeBounds]
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

    val pt = param.binder
    val constr1 = if (isRemovable(pt, param.paramNum)) remove(pt) else updated(param, tp)
    val result = new NaiveConstraint(constr1.myMap mapValues subst)
    if (Config.checkConstraintsNonCyclic) result.checkNonCyclic()
    result
  }

  def replace(param: PolyParam, tp: Type)(implicit ctx: Context): This =
    tp.dealias.stripTypeVar match {
      case tp: PolyParam if this contains tp =>
        val bs = bounds(tp)
        if (tp == param)
          this
        else if (param.occursIn(bs.lo, fromBelow = true) ||
                 param.occursIn(bs.hi, fromBelow = false))
          unify(tp, param).uncheckedReplace(param, tp)
        else
          uncheckedReplace(param, tp)
      case _ =>
        uncheckedReplace(param, tp)
    }

  def unify(p1: PolyParam, p2: PolyParam)(implicit ctx: Context): This = {
    val p1Bounds =
      dropParamIn(bounds(p1), p2.binder, p2.paramNum) &
      dropParamIn(bounds(p2), p1.binder, p1.paramNum)
    this.updated(p1, p1Bounds).updated(p2, TypeAlias(p1))
  }

  def add(poly: PolyType, tvars: List[TypeVar])(implicit ctx: Context): This = {
    val nparams = poly.paramNames.length
    val entries = new Array[Type](nparams * 2)
    poly.paramBounds.copyToArray(entries, 0)
    tvars.copyToArray(entries, nparams)
    updateEntries(poly, entries)
  }

  def domainPolys: List[PolyType] = myMap.keys

  def domainParams: List[PolyParam] =
    for {
      (poly, entries) <- myMap.toList
      n <- 0 until paramCount(entries)
      if isBounds(entries(n))
    } yield PolyParam(poly, n)

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

  private var myUninstVars: mutable.ArrayBuffer[TypeVar] = null

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

  def constrainedTypesText(printer: Printer): Text =
    Text(domainPolys map (_.toText(printer)), ", ")

  def constraintText(indent: Int, printer: Printer): Text = {
    val assocs =
      for (param <- domainParams)
      yield (" " * indent) ~ param.toText(printer) ~ at(param).toText(printer)
    Text(assocs, "\n")
  }

  override def toText(printer: Printer): Text = {
    val header: Text = "Constraint("
    val uninstVarsText = " uninstVars = " ~
      Text(uninstVars map (_.toText(printer)), ", ") ~ ";"
    val constrainedText =
      " constrained types = " ~ constrainedTypesText(printer) ~ ";"
    val constraintsText =
      " constraint = " ~ constraintText(3, printer) ~ ")"
    Text.lines(List(header, uninstVarsText, constrainedText, constraintsText))
  }
}
