package dotty.tools
package dotc
package core

import Contexts.*, Decorators.*, Symbols.*, Types.*
import NameKinds.UniqueName
import config.Printers.{gadts, gadtsConstr}
import util.{SimpleIdentitySet, SimpleIdentityMap}
import printing.*

import scala.annotation.tailrec
import scala.annotation.internal.sharable
import scala.collection.mutable

object GadtConstraint:
  @sharable val empty: GadtConstraint =
    GadtConstraint(OrderingConstraint.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty, false)

/** Represents GADT constraints currently in scope */
class GadtConstraint private (
  private val myConstraint: Constraint,
  private val mapping: SimpleIdentityMap[Symbol, TypeVar],
  private val reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
  private val wasConstrained: Boolean,
) extends Showable:
  def constraint: Constraint        = myConstraint
  def symbols: List[Symbol]         = mapping.keys
  def withConstraint(c: Constraint) = copy(myConstraint = c)
  def withWasConstrained            = copy(wasConstrained = true)

  def add(sym: Symbol, tv: TypeVar): GadtConstraint = copy(
    mapping        = mapping.updated(sym, tv),
    reverseMapping = reverseMapping.updated(tv.origin, sym),
  )

  def replace(param: TypeParamRef, tp: Type)(using Context) =
    var constr = constraint
    for
      poly <- constraint.domainLambdas
      paramRef <- poly.paramRefs
    do
      val entry0 = constr.entry(paramRef)
      val entry1 = entry0.substParam(param, tp)
      if entry1 ne entry0 then
        constr = constr.updateEntry(paramRef, entry1)
    withConstraint(constr)

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    constraint.isLess(tvarOrError(sym1).origin, tvarOrError(sym2).origin)

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(using Context): TypeBounds | Null = mapping(sym) match
    case null => null
    case tv: TypeVar => fullBounds(tv.origin) // .ensuring(containsNoInternalTypes(_))

  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(using Context): TypeBounds | Null =
    mapping(sym) match
      case null => null
      case tv: TypeVar =>
        def retrieveBounds: TypeBounds = externalize(constraint.bounds(tv.origin)).bounds
        retrieveBounds
          //.showing(i"gadt bounds $sym: $result", gadts)
          //.ensuring(containsNoInternalTypes(_))

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(using Context): Boolean = mapping(sym) != null

  /** GADT constraint narrows bounds of at least one variable */
  def isNarrowing: Boolean = wasConstrained

  def fullBounds(param: TypeParamRef)(using Context): TypeBounds =
    nonParamBounds(param).derivedTypeBounds(fullLowerBound(param), fullUpperBound(param))

  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    externalize(constraint.nonParamBounds(param)).bounds

  def fullLowerBound(param: TypeParamRef)(using Context): Type =
    val self = externalize(param)
    constraint.minLower(param).foldLeft(nonParamBounds(param).lo) { (acc, loParam) =>
      externalize(loParam) match
        // drop any lower param that is a GADT symbol
        // and is upper-bounded by the original parameter
        // e.g. in pos/i14287.min:
        //   case Foo.Bar[B$1](Foo.Bar[B$2](x)) => Foo.Bar(x)
        // after pattern type constraining:
        // B$1 had info <: X   and fullBounds >: B$2 <: X, and
        // B$2 had info <: B$1 and fullBounds <: B$1
        // If we keep these fullBounds, it would be a bidirectional definition.
        // So instead we can use the info of B$2 to drop the lower-bound of B$1
        // and return non-bidirectional bounds B$1 <: X and B$2 <: B$1.
        case lo: TypeRef if lo.symbol.isPatternBound && lo.info.hiBound.frozen_<:<(self) => acc
        case lo => acc | lo
    }

  def fullUpperBound(param: TypeParamRef)(using Context): Type =
    val self = externalize(param)
    constraint.minUpper(param).foldLeft(nonParamBounds(param).hi) { (acc, hiParam) =>
      externalize(hiParam) match
        case hi: TypeRef if hi.symbol.isPatternBound && self.frozen_<:<(hi.info.loBound) => acc // like fullLowerBound
        case hi =>
          // Any as the upper bound means "no bound", but if F is higher-kinded,
          // Any & F = F[_]; this is wrong for us so we need to short-circuit
          if acc.isAny then hi else acc & hi
    }

  def externalize(tp: Type, theMap: TypeMap | Null = null)(using Context): Type = tp match
    case param: TypeParamRef => reverseMapping(param) match
      case sym: Symbol => sym.typeRef
      case null        => param
    case tp: TypeAlias       => tp.derivedAlias(externalize(tp.alias, theMap))
    case tp                  => (if theMap == null then ExternalizeMap() else theMap).mapOver(tp)

  private class ExternalizeMap(using Context) extends TypeMap:
    def apply(tp: Type): Type = externalize(tp, this)(using mapCtx)

  def tvarOrError(sym: Symbol)(using Context): TypeVar =
    mapping(sym).ensuring(_ != null, i"not a constrainable symbol: $sym").uncheckedNN

  @tailrec final def stripInternalTypeVar(tp: Type): Type = tp match
    case tv: TypeVar =>
      val inst = constraint.instType(tv)
      if inst.exists then stripInternalTypeVar(inst) else tv
    case _ => tp

  def internalize(tp: Type)(using Context): Type = tp match
    case nt: NamedType =>
      val ntTvar = mapping(nt.symbol)
      if ntTvar == null then tp
      else ntTvar
    case _ => tp

  private def containsNoInternalTypes(tp: Type, theAcc: TypeAccumulator[Boolean] | Null = null)(using Context): Boolean = tp match {
    case tpr: TypeParamRef => !reverseMapping.contains(tpr)
    case tv: TypeVar => !reverseMapping.contains(tv.origin)
    case tp =>
      (if (theAcc != null) theAcc else new ContainsNoInternalTypesAccumulator()).foldOver(true, tp)
  }

  private class ContainsNoInternalTypesAccumulator(using Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean = x && containsNoInternalTypes(tp, this)
  }

  override def toText(printer: Printer): Texts.Text = printer.toText(this)

  /** Provides more information than toText, by showing the underlying Constraint details. */
  def debugBoundsDescription(using Context): String = i"$this\n$constraint"

  private def copy(
    myConstraint: Constraint = myConstraint,
    mapping: SimpleIdentityMap[Symbol, TypeVar] = mapping,
    reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol] = reverseMapping,
    wasConstrained: Boolean = wasConstrained,
  ): GadtConstraint = GadtConstraint(myConstraint, mapping, reverseMapping, wasConstrained)
end GadtConstraint

object GadtState:
  def apply(gadt: GadtConstraint): GadtState = ProperGadtState(gadt)

sealed trait GadtState {
  this: ConstraintHandling => // Hide ConstraintHandling within GadtConstraintHandling

  def gadt: GadtConstraint
  def gadt_=(g: GadtConstraint): Unit

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
          tp match
            case tp @ AndType(tp1, tp2) if !isUpper =>
              tp.derivedAndType(loop(tp1), loop(tp2))
            case tp @ OrType(tp1, tp2) if isUpper =>
              tp.derivedOrType(loop(tp1), loop(tp2))
            case tp: NamedType =>
              params.indexOf(tp.symbol) match
                case -1 =>
                  gadt.internalize(tp) match
                    case tv: TypeVar => tv.origin
                    case _ => tp
                case i => pt.paramRefs(i)
            case tp => tp
        }
        if !param.info.exists then
          throw TypeError(em"illegal recursive reference involving $param")
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
      gadt = gadt.add(sym, tv)
      tv
    }

    // The replaced symbols are picked up here.
    addToConstraint(poly1, tvars)
      .showing(i"added to constraint: [$poly1] $params%, % gadt = $gadt", gadts)
  }

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    val symTvar: TypeVar = gadt.stripInternalTypeVar(gadt.tvarOrError(sym)) match
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $sym -> $inst")
        return if isUpper then isSub(inst, bound) else isSub(bound, inst)

    val internalizedBound = gadt.stripInternalTypeVar(gadt.internalize(bound))

    val saved = constraint
    val result = internalizedBound match
      case boundTvar: TypeVar =>
        if boundTvar eq symTvar then true
        else if isUpper
        then addLess(symTvar.origin, boundTvar.origin)
        else addLess(boundTvar.origin, symTvar.origin)
      case bound =>
        addBoundTransitively(symTvar.origin, bound, isUpper)

    gadts.println {
      val descr = if isUpper then "upper" else "lower"
      val op = if isUpper then "<:" else ">:"
      i"adding $descr bound $sym $op $bound = $result"
    }

    if constraint ne saved then gadt = gadt.withWasConstrained
    result
  }

  def replace(param: TypeParamRef, tp: Type)(using Context) =
    gadt = gadt.replace(param, tp)

  /** See [[ConstraintHandling.approximation]] */
  def approximation(sym: Symbol, fromBelow: Boolean, maxLevel: Int = Int.MaxValue)(using Context): Type = {
    approximation(gadt.tvarOrError(sym).origin, fromBelow, maxLevel).match
      case tpr: TypeParamRef =>
        // Here we do externalization when the returned type is a TypeParamRef,
        //  b/c ConstraintHandling.approximation may return internal types when
        //  the type variable is instantiated. See #15531.
        gadt.externalize(tpr)
      case tp => tp
      .showing(i"approximating $sym ~> $result", gadts)
  }

  def fresh: GadtState = GadtState(gadt)

  /** Restore the GadtConstraint state. */
  def restore(gadt: GadtConstraint): Unit = this.gadt = gadt

  inline def rollbackGadtUnless(inline op: Boolean): Boolean =
    val saved = gadt
    var result = false
    try result = op
    finally if !result then restore(saved)
    result

  // ---- Protected/internal -----------------------------------------------

  override protected def constraint = gadt.constraint
  override protected def constraint_=(c: Constraint) = gadt = gadt.withConstraint(c)

  override protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSubType(tp1, tp2)
  override protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSameType(tp1, tp2)

  override def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds = gadt.nonParamBounds(param)
  override def fullLowerBound(param: TypeParamRef)(using Context): Type = gadt.fullLowerBound(param)
  override def fullUpperBound(param: TypeParamRef)(using Context): Type = gadt.fullUpperBound(param)

  // ---- Debug ------------------------------------------------------------

  override def constr = gadtsConstr
}

// Hide ConstraintHandling within GadtState
private class ProperGadtState(private var myGadt: GadtConstraint) extends ConstraintHandling with GadtState:
  def gadt: GadtConstraint               = myGadt
  def gadt_=(gadt: GadtConstraint): Unit = myGadt = gadt
