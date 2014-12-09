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

object Constraint {

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
}

import Constraint._

/** Constraint over undetermined type parameters
 *  @param myMap a map from PolyType to arrays.
 *               Each array contains twice the number of entries as there a type parameters
 *               in the PolyType. The first half of the array contains the type bounds that constrain the
 *               polytype's type parameters. The second half might contain type variables that
 *               track the corresponding parameters, or is left empty (filled with nulls).
 *               An instantiated type parameter is represented by having its instance type in
 *               the corresponding array entry.
 *  @param dependents a map from PolyTypes to arrays of Sets of PolyParams.
 *               The i'th set in an array corresponding to polytype `pt` contains
 *               those dependent `PolyParam`s `dp` that have `PolyParam(pt, i)` in their bounds in
 *               significant position. A position is significant if solving the
 *               constraint for `(pt, i)` with a type higher than its lower bound
 *               would lead to a constraint for `dp` that was not looser than
 *               the existing constraint. Specifically, it means that all poly params
 *               appearing covariantly in the lower bound and contravariantly in the
 *               upper bound, as well as all poly params appearing nonvariantly are
 *               significant.
 *               The `dependents` map is maintained and queried only of `Config.trackConstrDeps` is set.
 */
class Constraint(private val myMap: ParamInfo,
                 private val dependents: DependentMap) extends Showable {

  /** Does the constraint's domain contain the type parameters of `pt`? */
  def contains(pt: PolyType): Boolean = myMap(pt) != null

  /** Does the constraint's domain contain the type parameter `param`? */
  def contains(param: PolyParam): Boolean = {
    val entries = myMap(param.binder)
    entries != null && entries(param.paramNum).isInstanceOf[TypeBounds]
  }

  /** Does this constraint contain the type variable `tvar` and is it uninstantiated? */
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

  /** The constraint for given type parameter `param`, or NoType if `param` is not part of
   *  the constraint domain.
   */
  def at(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType else entries(param.paramNum)
  }

  /** The constraint bounds for given type parameter `param`.
   *  @pre `param` is not part of the constraint domain.
   */
  def bounds(param: PolyParam): TypeBounds = at(param).asInstanceOf[TypeBounds]

  /** The type variable corresponding to parameter `param`, or
   *  NoType, if `param` is not in constrained or is not paired with a type variable.
   */
  def typeVarOfParam(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType
    else {
      val tvar = typeVar(entries, param.paramNum)
      if (tvar != null) tvar else NoType
    }
  }

  /** Change dependencies in map `deps` to reflect new parameter bounds.
   *  @param deps       The map to change
   *  @param pt         the polytype that contains the parameters which might have new bounds
   *  @param entries    the entries for the parameters which might have new bounds
   *  @param delta      the change operation, one of `addDep` or `removeDep`.
   *  @param cmpEntries the comparison entries or `null` if no such entries exist.
   *                    As an optimization, only bounds that differ between `entries`
   *                    and `cmpEntries` will record their dependencies.
   */
  def changeDependencies(deps: DependentMap, pt: PolyType, entries: Array[Type], delta: DepDelta, cmpEntries: Array[Type])(implicit ctx: Context): DependentMap = {
    val limit = paramCount(entries)
    def loop(deps: DependentMap, n: Int): DependentMap = {
      if (n >= limit) deps
      else {
        val newDeps = entries(n) match {
          case bounds @ TypeBounds(lo, hi) =>
            val cmpBounds =
              if (cmpEntries == null) NoTypeBounds
              else cmpEntries(n) match {
                case bounds: TypeBounds => bounds
                case _ => NoTypeBounds
              }
            if (cmpBounds eq bounds) deps
            else {
              val param = PolyParam(pt, n)
              val deps1 =
                if (cmpBounds.lo eq lo) deps
                else new ChangeDependencies(param, 1, delta).apply(deps, lo)
              val deps2 =
                if (cmpBounds.hi eq hi) deps1
                else new ChangeDependencies(param, -1, delta).apply(deps1, hi)
              deps2
            }
          case _ =>
            deps
        }
        loop(newDeps, n + 1)
      }
    }
    if (Config.trackConstrDeps) loop(deps, 0) else deps
  }

  /** Change dependencies to reflect all changes between the bounds in `oldMap` and `newMap`.
   */
  def diffDependencies(deps: DependentMap, oldMap: ParamInfo, newMap: ParamInfo)(implicit ctx: Context): DependentMap =
    if (Config.trackConstrDeps) {
      var d = deps
      oldMap foreachBinding { (poly, entries) =>
        val newEntries = newMap(poly)
        if (newEntries ne entries) d = changeDependencies(d, poly, entries, removeDep, newEntries)
      }
      newMap foreachBinding { (poly, entries) =>
        val oldEntries = oldMap(poly)
        if (oldEntries ne entries) d = changeDependencies(d, poly, entries, addDep, oldEntries)
      }
      d
    } else deps

  /** The set of parameters that depend directly on `param`
   *  according to what's stored in `dependents`.
   */
  def dependentParams(param: PolyParam): Set[PolyParam] =
    safeSelect(dependents(param.binder), param.paramNum)

  /** A new constraint which is derived from this constraint by adding or replacing
   *  the entries corresponding to `pt` with `entries`.
   */
  private def updateEntries(pt: PolyType, entries: Array[Type])(implicit ctx: Context) : Constraint = {
    val res = new Constraint(
      myMap.updated(pt, entries),
      changeDependencies(dependents, pt, entries, addDep, myMap(pt)))
      
    //assert(res.domainPolys.filter(pt =>
    //  pt.resultType.resultType.widen.classSymbol.name.toString == "Ensuring").length < 2) //DEBUG
    if (Config.checkConstraintsNonCyclic) checkNonCyclic(pt, entries)
    ctx.runInfo.recordConstraintSize(res, res.myMap.size)
    res
  }

  /** Check that no constrained parameter in `pt` contains itself as a bound */
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

  /** Check that no constrained parameter contains itself as a bound */
  def checkNonCyclic()(implicit ctx: Context): Unit = {
    for (pt <- domainPolys) checkNonCyclic(pt, myMap(pt))
  }

  /** A new constraint which is derived from this constraint by updating
   *  the entry for parameter `param` to `tpe`.
   *  @pre  `this contains param`.
   */
  def updated(param: PolyParam, tpe: Type)(implicit ctx: Context): Constraint = {
    val newEntries = myMap(param.binder).clone
    newEntries(param.paramNum) = tpe
    updateEntries(param.binder, newEntries)
  }

  /** A new constraint which is derived from this constraint by mapping
   *  `op` over all entries of `poly`.
   *  @pre  `this contains poly`.
   */
  def transformed(poly: PolyType, op: Type => Type)(implicit ctx: Context) : Constraint =
    updateEntries(poly, myMap(poly) map op)

  /** A new constraint with all entries coming from `pt` removed. */
  def remove(pt: PolyType)(implicit ctx: Context) =
    new Constraint(
      myMap remove pt,
      changeDependencies(dependents, pt, myMap(pt), removeDep, null))

  /** Is entry associated with `pt` removable?
   *  @param removedParam The index of a parameter which is still present in the
   *                      entry array, but is going to be removed at the same step,
   *                      or -1 if no such parameter exists.
   */
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
  def dropParamIn(bounds: TypeBounds, poly: PolyType, n: Int)(implicit ctx: Context): TypeBounds = {
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
  private def uncheckedReplace(param: PolyParam, tp: Type)(implicit ctx: Context): Constraint = {

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
    val substMap = constr1.myMap mapValues subst
    val result = new Constraint(
      substMap,
      diffDependencies(constr1.dependents, constr1.myMap, substMap))
    if (Config.checkConstraintsNonCyclic) result.checkNonCyclic()
    result
  }

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   *  `tp` is another polyparam, applies the necessary unifications to avoud a cyclic
   *  constraint.
   */
  def replace(param: PolyParam, tp: Type)(implicit ctx: Context): Constraint =
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

  /** A constraint resulting by adding p2 = p1 to this constraint, and at the same
   *  time transferring all bounds of p2 to p1
   */
  def unify(p1: PolyParam, p2: PolyParam)(implicit ctx: Context): Constraint = {
    val p1Bounds =
      dropParamIn(bounds(p1), p2.binder, p2.paramNum) &
      dropParamIn(bounds(p2), p1.binder, p1.paramNum)
    this.updated(p1, p1Bounds).updated(p2, TypeAlias(p1))
  }

  /** A new constraint which is derived from this constraint by adding
   *  entries for all type parameters of `poly`.
   */
  def add(poly: PolyType, tvars: List[TypeVar] = Nil)(implicit ctx: Context) : Constraint = {
    val nparams = poly.paramNames.length
    val entries = new Array[Type](nparams * 2)
    poly.paramBounds.copyToArray(entries, 0)
    tvars.copyToArray(entries, nparams)
    updateEntries(poly, entries)
  }

  /** The polytypes constrained by this constraint */
  def domainPolys: List[PolyType] = myMap.keys

  /** The polytype parameters constrained by this constraint */
  def domainParams: List[PolyParam] =
    for {
      (poly, entries) <- myMap.toList
      n <- 0 until paramCount(entries)
      if isBounds(entries(n))
    } yield PolyParam(poly, n)

  /** Check whether predicate holds for all parameters in constraint
   */
  def forallParams(p: PolyParam => Boolean): Boolean = {
    myMap.foreachBinding { (poly, entries) =>
      for (i <- 0 until paramCount(entries))
        if (isBounds(entries(i)) && !p(PolyParam(poly, i))) return false
    }
    true
  }

  /** Perform operation `op` on all typevars, or only on uninstantiated
   *  typevars, depending on whether `uninstOnly` is set or not.
   */
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

trait ConstraintRunInfo { self: RunInfo =>
  private var maxSize = 0
  private var maxConstraint: Constraint = _
  def recordConstraintSize(c: Constraint, size: Int) =
    if (size > maxSize) {
      maxSize = size
      maxConstraint = c
    }
  def printMaxConstraint()(implicit ctx: Context) =
    if (maxSize > 0) typr.println(s"max constraint = ${maxConstraint.show}")
}
