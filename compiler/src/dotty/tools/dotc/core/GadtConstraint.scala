package dotty.tools
package dotc
package core

import Decorators._
import Contexts._
import Types._
import Symbols._
import util.SimpleIdentityMap
import collection.mutable
import printing._
import TypeOps.abstractTypeMemberSymbols

import scala.annotation.internal.sharable

/** Represents GADT constraints currently in scope */
sealed abstract class GadtConstraint extends Showable {
  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(using Context): TypeBounds
  def bounds(tp: TypeRef)(using Context): TypeBounds

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(using Context): TypeBounds
  def fullBounds(tp: TypeRef)(using Context): TypeBounds

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean

  /** Add symbols to constraint, correctly handling inter-dependencies.
   *
   * @see [[ConstraintHandling.addToConstraint]]
   */
  def addToConstraint(syms: List[Symbol])(using Context): Boolean
  def addToConstraint(sym: Symbol)(using Context): Boolean = addToConstraint(sym :: Nil)

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean
  def addBound(tpr: TypeRef, bound: Type, isUpper: Boolean)(using Context): Boolean

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(using Context): Boolean
  def contains(tp: TypeRef)(using Context): Boolean

  /** Is the type a constrainable path-dependent type?
   */
  def isConstrainablePDT(tp: Type)(using Context): Boolean

  /** Add path-dependent type to constraint.
   */
  def addPDT(tp: Type)(using Context): Boolean

  def isEmpty: Boolean
  final def nonEmpty: Boolean = !isEmpty

  /** See [[ConstraintHandling.approximation]] */
  def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type

  def fresh: GadtConstraint

  /** Restore the state from other [[GadtConstraint]], probably copied using [[fresh]] */
  def restore(other: GadtConstraint): Unit

  def debugBoundsDescription(using Context): String
}

final class ProperGadtConstraint private(
  private var myConstraint: Constraint,
  private var mapping: SimpleIdentityMap[TypeRef, TypeVar],
  private var reverseMapping: SimpleIdentityMap[TypeParamRef, TypeRef],
  private var tempMapping: SimpleIdentityMap[Symbol, TypeVar]
) extends GadtConstraint with ConstraintHandling {
  import dotty.tools.dotc.config.Printers.{gadts, gadtsConstr}

  def this() = this(
    myConstraint = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty),
    mapping = SimpleIdentityMap.empty,
    reverseMapping = SimpleIdentityMap.empty,
    tempMapping =  SimpleIdentityMap.empty
  )

  /** Exposes ConstraintHandling.subsumes */
  def subsumes(left: GadtConstraint, right: GadtConstraint, pre: GadtConstraint)(using Context): Boolean = {
    def extractConstraint(g: GadtConstraint) = g match {
      case s: ProperGadtConstraint => s.constraint
      case EmptyGadtConstraint => OrderingConstraint.empty
    }
    subsumes(extractConstraint(left), extractConstraint(right), extractConstraint(pre))
  }

  override def isConstrainablePDT(tp: Type)(using Context): Boolean = tp match
    case tp @ TypeRef(prefix, des) => isConstrainablePath(prefix) && ! tp.symbol.is(Flags.Opaque)
    case _ => false

  /** Whether type members of the given path is constrainable?
   *
   * Package's and module's type members will not be constrained.
   */
  private def isConstrainablePath(path: Type)(using Context): Boolean = path match
    case path: TermRef if !path.symbol.is(Flags.Package) && !path.symbol.is(Flags.Module) => true
    case _ => false

  override def addPDT(tp: Type)(using Context): Boolean =
    assert(isConstrainablePDT(tp), i"Type $tp is not a constrainable path-dependent type.")
    tp match
      case TypeRef(prefix: TermRef, _) => addTypeMembersOf(prefix, isUnamedPattern = false).nonEmpty
      case _ => false

  /** Find all constrainable type member symbols of the given type.
   *
   * All abstract but not opaque type members are returned.
   */
  private def constrainableTypeMemberSymbols(tp: Type)(using Context) =
    abstractTypeMemberSymbols(tp) filterNot (_.is(Flags.Opaque))

  private def addTypeMembersOf(path: Type, isUnamedPattern: Boolean)(using Context): Option[Map[Symbol, TypeVar]] =
    import NameKinds.DepParamName

    if !isUnamedPattern && !isConstrainablePath(path) then return None

    val pathType = if isUnamedPattern then path else path.widen
    val typeMembers = constrainableTypeMemberSymbols(pathType)

    if typeMembers.isEmpty then return Some(Map.empty)

    val poly1 = PolyType(typeMembers map { s => DepParamName.fresh(s.name.toTypeName) })(
      pt => typeMembers map { typeMember =>
        def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
          def loop(tp: Type): Type = tp match
            case tp @ AndType(tp1, tp2) if !isUpper =>
              tp.derivedAndOrType(loop(tp1), loop(tp2))
            case tp @ OrType(tp1, tp2) if isUpper =>
              tp.derivedOrType(loop(tp1), loop(tp2))
            case tp @ TypeRef(prefix, des) if prefix == pathType =>
              typeMembers indexOf tp.symbol match
                case -1 => tp
                case idx => pt.paramRefs(idx)
            case tp @ TypeRef(_: RecThis, des) =>
              typeMembers indexOf tp.symbol match
                case -1 => tp
                case idx => pt.paramRefs(idx)
            case tp: TypeRef =>
              mapping(tp) match {
                case tv: TypeVar => tv.origin
                case null => tp
              }
            case tp => tp

          loop(tp)
        }

        val tb = typeMember.info.bounds
        tb.derivedTypeBounds(
          lo = substDependentSyms(tb.lo, isUpper = false),
          hi = substDependentSyms(tb.hi, isUpper = true)
        )
      },
      pt => defn.AnyType
    )

    val tvars = typeMembers lazyZip poly1.paramRefs map { (sym, paramRef) =>
      val tv = TypeVar(paramRef, creatorState = null)

      if isUnamedPattern then
        tempMapping = tempMapping.updated(sym, tv)
      else
        val externalType = TypeRef(path, sym)
        mapping = mapping.updated(externalType, tv)
        reverseMapping = reverseMapping.updated(tv.origin, externalType)

      tv
    }

    def register =
      addToConstraint(poly1, tvars)
        .showing(i"added to constraint: [$poly1] $typeMembers%, %\n$debugBoundsDescription", gadts)

    if register then
      Some(Map.from(typeMembers lazyZip tvars))
    else
      None
  end addTypeMembersOf

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = {
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
                  mapping(tp.symbol.typeRef) match {
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
      mapping = mapping.updated(sym.typeRef, tv)
      reverseMapping = reverseMapping.updated(tv.origin, sym.typeRef)
      tv
    }

    // The replaced symbols are picked up here.
    addToConstraint(poly1, tvars)
      .showing(i"added to constraint: [$poly1] $params%, %\n$debugBoundsDescription", gadts)
  }

  override def addBound(tpr: TypeRef, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = constraint.instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvarOrError(tpr)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $tpr -> $inst")
        return if (isUpper) isSub(inst, bound) else isSub(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: NamedType =>
        val ntTvar = mapping(nt.symbol.typeRef)
        if (ntTvar ne null) stripInternalTypeVar(ntTvar) else bound
      case _ => bound
    }
    (
      internalizedBound match {
        case boundTvar: TypeVar =>
          if (boundTvar eq symTvar) true
          else if (isUpper) addLess(symTvar.origin, boundTvar.origin)
          else addLess(boundTvar.origin, symTvar.origin)
        case bound =>
          addBoundTransitively(symTvar.origin, bound, isUpper)
      }
    ).showing({
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $tpr $op $bound = $result"
    }, gadts)
  }

  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean =
    addBound(sym.typeRef, bound, isUpper)

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    constraint.isLess(tvarOrError(sym1).origin, tvarOrError(sym2).origin)

  override def fullBounds(tp: TypeRef)(using Context): TypeBounds =
    mapping(tp) match {
      case null => null
      case tv =>
        fullBounds(tv.origin)
          // .ensuring(containsNoInternalTypes(_))
    }

  override def fullBounds(sym: Symbol)(using Context): TypeBounds = fullBounds(sym.typeRef)

  override def bounds(tp: TypeRef)(using Context): TypeBounds =
    mapping(tp) match {
      case null => null
      case tv =>
        def retrieveBounds: TypeBounds =
          bounds(tv.origin) match {
            case TypeAlias(tpr: TypeParamRef) if reverseMapping.contains(tpr) =>
              TypeAlias(reverseMapping(tpr))
            case tb => tb
          }
        retrieveBounds
          //.showing(i"gadt bounds $sym: $result", gadts)
          //.ensuring(containsNoInternalTypes(_))
    }

  override def bounds(sym: Symbol)(using Context): TypeBounds = bounds(sym.typeRef)

  override def contains(tp: TypeRef)(using Context): Boolean = mapping(tp) ne null
  override def contains(sym: Symbol)(using Context): Boolean = contains(sym.typeRef)

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = {
    val res = approximation(tvarOrError(sym).origin, fromBelow = fromBelow)
    gadts.println(i"approximating $sym ~> $res")
    res
  }

  override def fresh: GadtConstraint = new ProperGadtConstraint(
    myConstraint,
    mapping,
    reverseMapping,
    tempMapping
  )

  def restore(other: GadtConstraint): Unit = other match {
    case other: ProperGadtConstraint =>
      this.myConstraint = other.myConstraint
      this.mapping = other.mapping
      this.reverseMapping = other.reverseMapping
      this.tempMapping = other.tempMapping
    case _ => ;
  }

  override def isEmpty: Boolean = mapping.size == 0

  // ---- Protected/internal -----------------------------------------------

  override protected def constraint = myConstraint
  override protected def constraint_=(c: Constraint) = myConstraint = c

  override protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSubType(tp1, tp2)
  override protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSameType(tp1, tp2)

  override def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    val externalizeMap = new TypeMap {
      def apply(tp: Type): Type = tp match {
        case tpr: TypeParamRef => externalize(tpr)
        case tp => mapOver(tp)
      }
    }
    externalizeMap(constraint.nonParamBounds(param)).bounds

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

  private def externalize(param: TypeParamRef)(using Context): Type =
    reverseMapping(param) match {
      case tpr: TypeRef => tpr
      case null => param
    }

  private def tvarOrError(tpr: TypeRef)(using Context): TypeVar =
    mapping(tpr).ensuring(_ ne null, i"not a constrainable type: $tpr")

  private def tvarOrError(sym: Symbol)(using Context): TypeVar = tvarOrError(sym.typeRef)

  private def containsNoInternalTypes(
    tp: Type,
    acc: TypeAccumulator[Boolean] = null
  )(using Context): Boolean = tp match {
    case tpr: TypeParamRef => !reverseMapping.contains(tpr)
    case tv: TypeVar => !reverseMapping.contains(tv.origin)
    case tp =>
      (if (acc ne null) acc else new ContainsNoInternalTypesAccumulator()).foldOver(true, tp)
  }

  private class ContainsNoInternalTypesAccumulator(using Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean = x && containsNoInternalTypes(tp)
  }

  // ---- Debug ------------------------------------------------------------

  override def constr = gadtsConstr

  override def toText(printer: Printer): Texts.Text = constraint.toText(printer)

  override def debugBoundsDescription(using Context): String = {
    val sb = new mutable.StringBuilder
    sb ++= constraint.show
    sb += '\n'
    mapping.foreachBinding { case (tpr, _) =>
      sb ++= i"$tpr: ${fullBounds(tpr)}\n"
    }
    sb.result
  }
}

@sharable object EmptyGadtConstraint extends GadtConstraint {
  override def bounds(sym: Symbol)(using Context): TypeBounds = null
  override def fullBounds(sym: Symbol)(using Context): TypeBounds = null
  override def bounds(tp: TypeRef)(using Context): TypeBounds = null
  override def fullBounds(tp: TypeRef)(using Context): TypeBounds = null

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean = unsupported("EmptyGadtConstraint.isLess")

  override def isEmpty: Boolean = true

  override def contains(sym: Symbol)(using Context) = false
  override def contains(tp: TypeRef)(using Context) = false
  override def isConstrainablePDT(tp: Type)(using Context): Boolean = false
  override def addPDT(tp: Type)(using Context): Boolean = false

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = unsupported("EmptyGadtConstraint.addToConstraint")
  override def addBound(tpr: TypeRef, bound: Type, isUpper: Boolean)(using Context): Boolean = unsupported("EmptyGadtConstraint.addBound")
  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = unsupported("EmptyGadtConstraint.addBound")

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = unsupported("EmptyGadtConstraint.approximation")

  override def fresh = new ProperGadtConstraint
  override def restore(other: GadtConstraint): Unit =
    if (!other.isEmpty) sys.error("cannot restore a non-empty GADTMap")

  override def debugBoundsDescription(using Context): String = "EmptyGadtConstraint"

  override def toText(printer: Printer): Texts.Text = "EmptyGadtConstraint"
}
