package dotty.tools
package dotc
package core

import Decorators._
import Contexts._
import Types._
import Symbols._
import Names.Name
import util.SimpleIdentityMap
import collection.mutable
import printing._

import scala.annotation.internal.sharable

/** Represents GADT constraints currently in scope */
sealed abstract class GadtConstraint extends Showable {
  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(using Context): TypeBounds

  /** Simiar to [[bounds]], while retrieve bounds for type members. */
  def bounds(path: TermRef, designator: Name)(using Context): TypeBounds

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(using Context): TypeBounds
  def fullBounds(path: TermRef, designator: Name)(using Context): TypeBounds

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean

  /** Add symbols to constraint, correctly handling inter-dependencies.
   *
   * @see [[ConstraintHandling.addToConstraint]]
   */
  def addToConstraint(syms: List[Symbol])(using Context): Boolean
  def addToConstraint(sym: Symbol)(using Context): Boolean = addToConstraint(sym :: Nil)

  /** Add type members to constraint.
    */
  def addToConstraint(scrut: Type, pat: Type, scrutPath: TermRef, scrutTpMems: List[(Name, TypeBounds)], patTpMems: List[(Name, TypeBounds)], maybePatPath: Option[TermRef] = None)(using Context): Boolean

  def narrowScrutTp_=(tp: Type): Unit
  def narrowScrutTp: Type

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(using Context): Boolean

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
  private var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
  private var tpmMapping: SimpleIdentityMap[TermRef, SimpleIdentityMap[Name, TypeVar]],
  private var reverseTpmMapping: SimpleIdentityMap[TypeParamRef, TypeRef],
  private var myNarrowScrutTp: Type
) extends GadtConstraint with ConstraintHandling {
  import dotty.tools.dotc.config.Printers.{gadts, gadtsConstr}

  def this() = this(
    myConstraint = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty),
    mapping = SimpleIdentityMap.empty,
    reverseMapping = SimpleIdentityMap.empty,
    tpmMapping = SimpleIdentityMap.empty,
    reverseTpmMapping = SimpleIdentityMap.empty,
    myNarrowScrutTp = null
  )

  /** Exposes ConstraintHandling.subsumes */
  def subsumes(left: GadtConstraint, right: GadtConstraint, pre: GadtConstraint)(using Context): Boolean = {
    def extractConstraint(g: GadtConstraint) = g match {
      case s: ProperGadtConstraint => s.constraint
      case EmptyGadtConstraint => OrderingConstraint.empty
    }
    subsumes(extractConstraint(left), extractConstraint(right), extractConstraint(pre))
  }

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
      .showing(i"added to constraint: [$poly1] $params%, %\n$debugBoundsDescription", gadts)
  }

  override def addToConstraint(scrut: Type, pat: Type, scrutPath: TermRef, scrutTpMems: List[(Name, TypeBounds)], patTpMems: List[(Name, TypeBounds)], maybePatPath: Option[TermRef] = None)(using Context): Boolean = {
    import NameKinds.DepParamName
    // add type member names, from both scrutinee and pattern
    val tpmNames: List[Name] = { (scrutTpMems ++ patTpMems) map (_._1) }.distinct
    // type member names from the scrutinee
    val scrutNames: Set[Name] = Set.from { scrutTpMems map (_._1) }
    // type member names from the pattern
    val patNames: Set[Name] = Set.from { patTpMems map (_._1) }

    def maybeMapTpMem(path: Option[TermRef], designator: Name): TypeVar =
      path.map(path => mapTpMem(path, designator)).getOrElse(null)

    def nullEither[T <: AnyRef](x1: => T, x2: => T): T =
      if x1 eq null then
        x2
      else
        x1

    // classify names into two groups, those internalized before and those new
    val (internalizedNames, newNames): (List[Name], List[Name]) = {
      val m = tpmNames groupBy { name =>
       nullEither(mapTpMem(scrutPath, name), maybeMapTpMem(maybePatPath, name)) ne null
      }

      (m.getOrElse(true, Nil), m.getOrElse(false, Nil))
    }

    var equalTvars: List[(TypeVar, TypeVar)] = Nil

    val existingTvars: List[TypeVar] = internalizedNames map { name =>
      (mapTpMem(scrutPath, name), maybeMapTpMem(maybePatPath, name)) match {
        case (null, tvar2: TypeVar) =>
          if scrutNames contains name then {
            tpmMapping = tpmMapping.updated(
              scrutPath,
              tpmMapping(scrutPath) match {
                case null => SimpleIdentityMap.empty.updated(name, tvar2)
                case m => m.updated(name, tvar2)
              }
            )
            reverseTpmMapping = reverseTpmMapping.updated(tvar2.origin, TypeRef(scrutPath, name))
          }
          tvar2
        case (tvar1: TypeVar, null) =>
          if patNames contains name then {
            maybePatPath map { patPath =>
              tpmMapping = tpmMapping.updated(
                patPath,
                tpmMapping(patPath) match {
                  case null => SimpleIdentityMap.empty.updated(name, tvar1)
                  case m => m.updated(name, tvar1)
                }
              )
              reverseTpmMapping = reverseTpmMapping.updated(tvar1.origin, TypeRef(scrutPath, name))
            }
          }
          tvar1
        case (tvar1: TypeVar, tvar2: TypeVar) =>
          equalTvars = tvar1 -> tvar2 :: equalTvars
          tvar1
      }
    }

    val poly1 =
      if newNames.nonEmpty then
        PolyType(newNames map { name => DepParamName.fresh(name.toTypeName) })(
          _ => newNames map { _ => TypeBounds(defn.NothingType, defn.AnyType) },
          _ => defn.AnyType
        )
      else
        null
    val newTvars =
      if newNames.nonEmpty then
        newNames.lazyZip(poly1.paramRefs).map { (name, paramRef) =>
          val tv = TypeVar(paramRef, creatorState = null)

          if scrutNames contains name then {
            tpmMapping = tpmMapping.updated(
              scrutPath,
              tpmMapping(scrutPath) match {
                case null => SimpleIdentityMap.empty.updated(name, tv)
                case m => m.updated(name, tv)
              }
            )
            reverseTpmMapping = reverseTpmMapping.updated(tv.origin, TypeRef(scrutPath, name))
          }

          if patNames contains name then {
            maybePatPath map { patPath =>
              tpmMapping = tpmMapping.updated(
                patPath,
                tpmMapping(patPath) match {
                  case null => SimpleIdentityMap.empty.updated(name, tv)
                  case m => m.updated(name, tv)
                }
              )
              reverseTpmMapping = reverseTpmMapping.updated(tv.origin, TypeRef(scrutPath, name))
            }
          }

          tv
        }
        else
          Nil

    // type member names, reordered
    val tpmNames1 = internalizedNames ++ newNames
    // type variables associated to type members, ordered the same way as tpmNames1
    val tvars = existingTvars ++ newTvars

    // build a map from member names to type vars
    val tvarOfName: Map[Name, TypeVar] = Map.from { tpmNames1 zip tvars }
    def getTvarOfName(name: Name): TypeVar = (tvarOfName get name).ensuring(_.isDefined, "member name is not in collected name list").get

    // substitute dependent symbols in tb
    def processBounds(tb: TypeBounds): TypeBounds = {
      def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
        def loop(tp: Type) = substDependentSyms(tp, isUpper)
        tp match {
          case tp @ AndType(tp1, tp2) if !isUpper =>
            tp.derivedAndType(loop(tp1), loop(tp2))
          case tp @ OrType(tp1, tp2) if isUpper =>
            tp.derivedOrType(loop(tp1), loop(tp2))
          case TypeRef(tp, des: Symbol) if tp == scrut || tp == pat =>
            getTvarOfName(des.name)
          case TypeRef(_ : RecThis, des : Symbol) =>
            getTvarOfName(des.name)
          case TypeRef(tp, des: Name) if tp == scrut || tp == pat =>
            getTvarOfName(des)
          case TypeRef(_ : RecThis, des : Name) =>
            getTvarOfName(des)
          case tp: NamedType =>
            mapping(tp.symbol) match {
              case tv: TypeVar => tv
              case null => tp
            }
          case tp => tp
        }
      }
      tb match {
        case alias : TypeAlias =>
          alias.derivedAlias(substDependentSyms(alias.lo, isUpper = false))
        case _ =>
          tb.derivedTypeBounds(
            lo = substDependentSyms(tb.lo, isUpper = false),
            hi = substDependentSyms(tb.hi, isUpper = true)
          )
      }
    }

    def addPolyOk: Boolean =
      (poly1 eq null) || addToConstraint(poly1, newTvars)
        .showing(i"added to constraint: [$poly1]%, %\n$debugBoundsDescription", gadts)

    def addUpperBound(tp1: Type, tp2: Type): Boolean = {
      (
        stripInternalTypeVar(tp1),
        stripInternalTypeVar(tp2)
      ) match {
        case (tv1 : TypeVar, t2) => addBound(tv1, t2, isUpper = true)
        case (t1, tv2: TypeVar) => addBound(tv2, t1, isUpper = false)
        case (t1, t2) => isSub(t1, t2)
      }
    }

    def addBoundsOk: List[Boolean] = (scrutTpMems ++ patTpMems) map { (name, tb) =>
      val tvar = getTvarOfName(name)
      val tb1 = processBounds(tb)

      {
        tb1.lo.isRef(defn.NothingClass) || addUpperBound(tb1.lo, tvar)
      } && {
        tb1.hi.isAny || addUpperBound(tvar, tb1.hi)
      }
    }

    def eqTvarsOk = equalTvars map { (tvar1, tvar2) =>
      addUpperBound(tvar1, tvar2) && addUpperBound(tvar2, tvar1)
    }

    addPolyOk && (addBoundsOk forall identity) && (eqTvarsOk forall identity)
  }

  override def narrowScrutTp_=(tp: Type): Unit = myNarrowScrutTp = tp
  override def narrowScrutTp: Type = myNarrowScrutTp

  @annotation.tailrec private def stripInternalTypeVar(tp: Type): Type = tp match {
    case tv: TypeVar =>
      val inst = constraint.instType(tv)
      if (inst.exists) stripInternalTypeVar(inst) else tv
    case _ => tp
  }

  private def addBound(tvar: TypeVar, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    val symTvar: TypeVar = stripInternalTypeVar(tvar) match {
      case tv: TypeVar => tv
      case inst =>
        // gadts.println(i"instantiated: $sym -> $inst")
        return if (isUpper) isSub(inst, bound) else isSub(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: NamedType =>
        val ntTvar = mapping(nt.symbol)
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
      i"adding $descr bound $tvar $op $bound = $result"
    }, gadts)
  }

  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean =
    addBound(tvarOrError(sym), bound, isUpper)

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    constraint.isLess(tvarOrError(sym1).origin, tvarOrError(sym2).origin)

  override def fullBounds(sym: Symbol)(using Context): TypeBounds =
    mapping(sym) match {
      case null => null
      case tv =>
        fullBounds(tv.origin)
          // .ensuring(containsNoInternalTypes(_))
    }

  override def fullBounds(path: TermRef, designator: Name)(using Context): TypeBounds =
    mapTpMem(path, designator) match {
      case null => null
      case tv =>
        fullBounds(tv.origin)
        // .ensuring(containsNoInternalTypes(_))
    }

  private def mapTpMem(path: TermRef, designator: Name): TypeVar = tpmMapping(path) match {
    case null => null
    case nameMapping => nameMapping(designator)
  }

  override def bounds(path: TermRef, designator: Name)(using Context): TypeBounds =
    mapTpMem(path, designator) match {
      case null => null
      case tv =>
        def retrieveBounds: TypeBounds =
          bounds(tv.origin) match {
            case TypeAlias(tpr: TypeParamRef) if reverseTpmMapping.contains(tpr) =>
              TypeAlias(reverseTpmMapping(tpr))
            case TypeAlias(tpr: TypeParamRef) if reverseMapping.contains(tpr) =>
              TypeAlias(reverseMapping(tpr).typeRef)
            case tb => tb
          }
        retrieveBounds
    }

  override def bounds(sym: Symbol)(using Context): TypeBounds =
    mapping(sym) match {
      case null => null
      case tv =>
        def retrieveBounds: TypeBounds =
          bounds(tv.origin) match {
            case TypeAlias(tpr: TypeParamRef) if reverseTpmMapping.contains(tpr) =>
              TypeAlias(reverseTpmMapping(tpr))
            case TypeAlias(tpr: TypeParamRef) if reverseMapping.contains(tpr) =>
              TypeAlias(reverseMapping(tpr).typeRef)
            case tb => tb
          }
        retrieveBounds
          //.showing(i"gadt bounds $sym: $result", gadts)
          //.ensuring(containsNoInternalTypes(_))
    }

  override def contains(sym: Symbol)(using Context): Boolean = mapping(sym) ne null

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = {
    val res = approximation(tvarOrError(sym).origin, fromBelow = fromBelow)
    gadts.println(i"approximating $sym ~> $res")
    res
  }

  override def fresh: GadtConstraint = new ProperGadtConstraint(
    myConstraint,
    mapping,
    reverseMapping,
    tpmMapping,
    reverseTpmMapping,
    myNarrowScrutTp
  )

  def restore(other: GadtConstraint): Unit = other match {
    case other: ProperGadtConstraint =>
      this.myConstraint = other.myConstraint
      this.mapping = other.mapping
      this.reverseMapping = other.reverseMapping
      this.tpmMapping = other.tpmMapping
      this.reverseTpmMapping = other.reverseTpmMapping
      this.myNarrowScrutTp = other.myNarrowScrutTp
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
      case sym: Symbol => sym.typeRef
      case null => param
    }

  private def tvarOrError(sym: Symbol)(using Context): TypeVar =
    mapping(sym).ensuring(_ ne null, i"not a constrainable symbol: $sym")

  private def tvarOrError(path: TermRef, designator: Name)(using Context): TypeVar = {
    def get: TypeVar =
      tpmMapping(path) match {
        case null => null
        case nameMapping => nameMapping(designator)
      }

    get.ensuring(_ ne null, i"not a constrainable path-dependent type: $path.$designator")
  }

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
    mapping.foreachBinding { case (sym, _) =>
      sb ++= i"$sym: ${fullBounds(sym)}\n"
    }
    tpmMapping.foreachBinding { case (path, nameMapping) =>
      nameMapping.foreachBinding { case (name, _) =>
        sb ++= i"(${path.symbol}).$name: ${fullBounds(path, name)}\n"
      }
    }
    sb.result
  }
}

@sharable object EmptyGadtConstraint extends GadtConstraint {
  override def bounds(sym: Symbol)(using Context): TypeBounds = null
  override def bounds(path: TermRef, designator: Name)(using Context): TypeBounds = null
  override def fullBounds(sym: Symbol)(using Context): TypeBounds = null
  override def fullBounds(path: TermRef, designator: Name)(using Context): TypeBounds = null

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean = unsupported("EmptyGadtConstraint.isLess")

  override def isEmpty: Boolean = true

  override def contains(sym: Symbol)(using Context) = false

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = unsupported("EmptyGadtConstraint.addToConstraint")
  override def addToConstraint(scrut: Type, pat: Type, scrutPath: TermRef, scrutTpMems: List[(Name, TypeBounds)], patTpMems: List[(Name, TypeBounds)], maybePatPath: Option[TermRef] = None)(using Context): Boolean =
    unsupported("EmptyGadtConstraint.addToConstraint")
  override def narrowScrutTp_=(tp: Type): Unit = unsupported("EmptyGadtConstraint.narrowScrutTp_=")
  override def narrowScrutTp: Type = unsupported("EmptyGadtConstraint.narrowScrutTp")
  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = unsupported("EmptyGadtConstraint.addBound")

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = unsupported("EmptyGadtConstraint.approximation")

  override def fresh = new ProperGadtConstraint
  override def restore(other: GadtConstraint): Unit =
    if (!other.isEmpty) sys.error("cannot restore a non-empty GADTMap")

  override def debugBoundsDescription(using Context): String = "EmptyGadtConstraint"

  override def toText(printer: Printer): Texts.Text = "EmptyGadtConstraint"
}
