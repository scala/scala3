package dotty.tools
package dotc
package core

import Decorators._
import Contexts._
import Types._
import Symbols._
import util.{SimpleIdentitySet, SimpleIdentityMap}
import collection.mutable
import printing._
import annotation.retains

object GadtConstraint:
  def apply(): GadtConstraint = empty
  def empty: GadtConstraint =
    new ProperGadtConstraint(OrderingConstraint.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty, false)

/** Represents GADT constraints currently in scope */
sealed trait GadtConstraint (
  private var myConstraint: Constraint,
  private var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
  private var wasConstrained: Boolean
) extends Showable {
  this: ConstraintHandling =>

  import dotty.tools.dotc.config.Printers.{gadts, gadtsConstr}

  /** Exposes ConstraintHandling.subsumes */
  def subsumes(left: GadtConstraint, right: GadtConstraint, pre: GadtConstraint)(using Context): Boolean = {
    def extractConstraint(g: GadtConstraint) = g.constraint
    subsumes(extractConstraint(left), extractConstraint(right), extractConstraint(pre))
  }

  override protected def legalBound(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Type =
    // GADT constraints never involve wildcards and are not propagated outside
    // the case where they're valid, so no approximating is needed.
    rawBound

  /** Add symbols to constraint, correctly handling inter-dependencies.
   *
   * @see [[ConstraintHandling.addToConstraint]]
   */
  def addToConstraint(sym: Symbol)(using Context): Boolean = addToConstraint(sym :: Nil)
  def addToConstraint(params: List[Symbol])(using Context): Boolean = {
    import NameKinds.DepParamName

    val poly1 = PolyType(params.map { sym => DepParamName.fresh(sym.name.toTypeName) })(
      pt => params.map { param =>
        // In bound type `tp`, replace the symbols in dependent positions with their internal TypeParamRefs.
        // The replaced symbols will be later picked up in `ConstraintHandling#addToConstraint`
        // and used as orderings.
        def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
          def loop(tp: Type) = substDependentSyms(tp, isUpper)
          tp match {
            case tp @ AndType(tp1, tp2) if !isUpper =>
              tp.derivedAndType(loop(tp1), loop(tp2))
            case tp @ OrType(tp1, tp2) if isUpper =>
              tp.derivedOrType(loop(tp1), loop(tp2))
            case tp: NamedType =>
              params.indexOf(tp.symbol) match {
                case -1 =>
                  mapping(tp.symbol) match {
                    case tv: TypeVar => tv.origin
                    case null => tp
                  }
                case i => pt.paramRefs(i)
              }
            case tp => tp
          }
        }

        val tb = param.info.bounds
        tb.derivedTypeBounds(
          lo = substDependentSyms(tb.lo, isUpper = false),
          hi = substDependentSyms(tb.hi, isUpper = true)
        )
      },
      pt => defn.AnyType
    )

    val tvars = params.lazyZip(poly1.paramRefs).map { (sym, paramRef) =>
      val tv = TypeVar(paramRef, creatorState = null)
      mapping = mapping.updated(sym, tv)
      reverseMapping = reverseMapping.updated(tv.origin, sym)
      tv
    }

    // The replaced symbols are picked up here.
    addToConstraint(poly1, tvars)
      .showing(i"added to constraint: [$poly1] $params%, % gadt = $this", gadts)(using null)
  }

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = constraint.instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvarOrError(sym)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $sym -> $inst")
        return if (isUpper) isSub(inst, bound) else isSub(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: NamedType =>
        val ntTvar = mapping(nt.symbol)
        if (ntTvar != null) stripInternalTypeVar(ntTvar) else bound
      case _ => bound
    }

    val saved = constraint
    val result = internalizedBound match
      case boundTvar: TypeVar =>
        if (boundTvar eq symTvar) true
        else if (isUpper) addLess(symTvar.origin, boundTvar.origin)
        else addLess(boundTvar.origin, symTvar.origin)
      case bound =>
        addBoundTransitively(symTvar.origin, bound, isUpper)

    gadts.println {
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $sym $op $bound = $result"
    }

    if constraint ne saved then wasConstrained = true
    result
  }

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    constraint.isLess(tvarOrError(sym1).origin, tvarOrError(sym2).origin)

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(using Context): TypeBounds | Null =
    mapping(sym) match {
      case null => null
      // TODO: Improve flow typing so that ascription becomes redundant
      case tv: TypeVar =>
        fullBounds(tv.origin)
          // .ensuring(containsNoInternalTypes(_))
    }

  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(using Context): TypeBounds | Null =
    mapping(sym) match {
      case null => null
      // TODO: Improve flow typing so that ascription becomes redundant
      case tv: TypeVar =>
        def retrieveBounds: TypeBounds = externalize(bounds(tv.origin)).bounds
        retrieveBounds
          //.showing(i"gadt bounds $sym: $result", gadts)
          //.ensuring(containsNoInternalTypes(_))
    }

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(using Context): Boolean = mapping(sym) != null

  /** GADT constraint narrows bounds of at least one variable */
  def isNarrowing: Boolean = wasConstrained

  /** See [[ConstraintHandling.approximation]] */
  def approximation(sym: Symbol, fromBelow: Boolean, maxLevel: Int = Int.MaxValue)(using Context): Type = {
    val res =
      approximation(tvarOrError(sym).origin, fromBelow, maxLevel) match
        case tpr: TypeParamRef =>
          // Here we do externalization when the returned type is a TypeParamRef,
          //  b/c ConstraintHandling.approximation may return internal types when
          //  the type variable is instantiated. See #15531.
          externalize(tpr)
        case tp => tp

    gadts.println(i"approximating $sym ~> $res")
    res
  }

  def symbols: List[Symbol] = mapping.keys

  def fresh: GadtConstraint = new ProperGadtConstraint(myConstraint, mapping, reverseMapping, wasConstrained)

  /** Restore the state from other [[GadtConstraint]], probably copied using [[fresh]] */
  def restore(other: GadtConstraint): Unit =
    this.myConstraint = other.myConstraint
    this.mapping = other.mapping
    this.reverseMapping = other.reverseMapping
    this.wasConstrained = other.wasConstrained

  // ---- Protected/internal -----------------------------------------------

  override protected def constraint = myConstraint
  override protected def constraint_=(c: Constraint) = myConstraint = c

  override protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSubType(tp1, tp2)
  override protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSameType(tp1, tp2)

  override def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    externalize(constraint.nonParamBounds(param)).bounds

  override def fullLowerBound(param: TypeParamRef)(using Context): Type =
    constraint.minLower(param).foldLeft(nonParamBounds(param).lo) {
      (t, u) => t | externalize(u)
    }

  override def fullUpperBound(param: TypeParamRef)(using Context): Type =
    constraint.minUpper(param).foldLeft(nonParamBounds(param).hi) { (t, u) =>
      val eu = externalize(u)
      // Any as the upper bound means "no bound", but if F is higher-kinded,
      // Any & F = F[_]; this is wrong for us so we need to short-circuit
      if t.isAny then eu else t & eu
    }

  // ---- Private ----------------------------------------------------------

  private def externalize(tp: Type, theMap: TypeMap @retains(caps.cap) | Null = null)(using Context): Type = tp match
    case param: TypeParamRef => reverseMapping(param) match
      case sym: Symbol => sym.typeRef
      case null        => param
    case tp: TypeAlias       => tp.derivedAlias(externalize(tp.alias, theMap))
    case tp                  => (if theMap == null then ExternalizeMap() else theMap).mapOver(tp)

  private class ExternalizeMap(using Context) extends TypeMap:
    def apply(tp: Type): Type = externalize(tp, this)(using mapCtx)

  private def tvarOrError(sym: Symbol)(using Context): TypeVar =
    mapping(sym).ensuring(_ != null, i"not a constrainable symbol: $sym").uncheckedNN

  private def containsNoInternalTypes(tp: Type, theAcc: TypeAccumulator[Boolean] @retains(caps.cap) | Null = null)(using Context): Boolean = tp match {
    case tpr: TypeParamRef => !reverseMapping.contains(tpr)
    case tv: TypeVar => !reverseMapping.contains(tv.origin)
    case tp =>
      (if (theAcc != null) theAcc else new ContainsNoInternalTypesAccumulator()).foldOver(true, tp)
  }

  private class ContainsNoInternalTypesAccumulator(using Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean = x && containsNoInternalTypes(tp, this)
  }

  // ---- Debug ------------------------------------------------------------

  override def constr = gadtsConstr

  override def toText(printer: Printer): Texts.Text = printer.toText(this)

  /** Provides more information than toText, by showing the underlying Constraint details. */
  def debugBoundsDescription(using Context): String = i"$this\n$constraint"
}

private class ProperGadtConstraint (
    myConstraint: Constraint,
    mapping: SimpleIdentityMap[Symbol, TypeVar],
    reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
    wasConstrained: Boolean,
) extends ConstraintHandling with GadtConstraint(myConstraint, mapping, reverseMapping, wasConstrained)
