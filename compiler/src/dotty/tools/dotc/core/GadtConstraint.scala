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

import scala.annotation.internal.sharable

/** Represents GADT constraints currently in scope */
sealed abstract class GadtConstraint extends Showable {
  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(implicit ctx: Context): TypeBounds

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(implicit ctx: Context): TypeBounds

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean

  /** Add symbols to constraint, correctly handling inter-dependencies.
   *
   * @see [[ConstraintHandling.addToConstraint]]
   */
  def addToConstraint(syms: List[Symbol])(implicit ctx: Context): Boolean
  def addToConstraint(sym: Symbol)(implicit ctx: Context): Boolean = addToConstraint(sym :: Nil)

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(implicit ctx: Context): Boolean

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(implicit ctx: Context): Boolean

  def isEmpty: Boolean

  /** See [[ConstraintHandling.approximation]] */
  def approximation(sym: Symbol, fromBelow: Boolean)(implicit ctx: Context): Type

  def fresh: GadtConstraint

  /** Restore the state from other [[GadtConstraint]], probably copied using [[fresh]] */
  def restore(other: GadtConstraint): Unit

  def debugBoundsDescription(implicit ctx: Context): String
}

final class ProperGadtConstraint private(
  private var myConstraint: Constraint,
  private var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
) extends GadtConstraint with ConstraintHandling[Context] {
  import dotty.tools.dotc.config.Printers.{gadts, gadtsConstr}

  def this() = this(
    myConstraint = new OrderingConstraint(SimpleIdentityMap.Empty, SimpleIdentityMap.Empty, SimpleIdentityMap.Empty),
    mapping = SimpleIdentityMap.Empty,
    reverseMapping = SimpleIdentityMap.Empty
  )

  /** Exposes ConstraintHandling.subsumes */
  def subsumes(left: GadtConstraint, right: GadtConstraint, pre: GadtConstraint)(implicit ctx: Context): Boolean = {
    def extractConstraint(g: GadtConstraint) = g match {
      case s: ProperGadtConstraint => s.constraint
      case EmptyGadtConstraint => OrderingConstraint.empty
    }
    subsumes(extractConstraint(left), extractConstraint(right), extractConstraint(pre))
  }

  override def addToConstraint(params: List[Symbol])(implicit ctx: Context): Boolean = {
    import NameKinds.DepParamName

    val poly1 = PolyType(params.map { sym => DepParamName.fresh(sym.name.toTypeName) })(
      pt => params.map { param =>
        // In bound type `tp`, replace the symbols in dependent positions with their internal TypeParamRefs.
        // The replaced symbols will be later picked up in `ConstraintHandling#addToConstraint`
        // and used as orderings.
        def substDependentSyms(tp: Type, isUpper: Boolean)(implicit ctx: Context): Type = {
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

    val tvars = (params, poly1.paramRefs).zipped.map { (sym, paramRef) =>
      val tv = new TypeVar(paramRef, creatorState = null)
      mapping = mapping.updated(sym, tv)
      reverseMapping = reverseMapping.updated(tv.origin, sym)
      tv
    }

    // The replaced symbols are picked up here.
    addToConstraint(poly1, tvars).reporting({ _ =>
      i"added to constraint: $params%, %\n$debugBoundsDescription"
    }, gadts)
  }

  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(implicit ctx: Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvarOrError(sym)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $sym -> $inst")
        return if (isUpper) isSubType(inst , bound) else isSubType(bound, inst)
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
          val oldUpperBound = bounds(symTvar.origin)
          // If we have bounds:
          //     F >: [t] => List[t] <: [t] => Any
          // and we want to record that: 
          //     F <: [+A] => List[A]
          // we need to adapt the variance and instead record that:
          //     F <: [A] => List[A]
          // We cannot record the original bound, since it is false that:
          //     [t] => List[t]  <:  [+A] => List[A]
          //
          // Note that the following code is accepted:
          //     class Foo[F[t] >: List[t]]
          //     type T = Foo[List]
          // precisely because Foo[List] is desugared to Foo[[A] => List[A]].
          //
          // Ideally we'd adapt the bound in ConstraintHandling#addOneBound,
          // but doing it there actually interferes with type inference.
          val bound1 = bound.adaptHkVariances(oldUpperBound)
          if (isUpper) addUpperBound(symTvar.origin, bound1)
          else addLowerBound(symTvar.origin, bound1)
      }
    ).reporting({ res =>
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $sym $op $bound = $res"
    }, gadts)
  }

  override def isLess(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean =
    constraint.isLess(tvarOrError(sym1).origin, tvarOrError(sym2).origin)

  override def fullBounds(sym: Symbol)(implicit ctx: Context): TypeBounds =
    mapping(sym) match {
      case null => null
      case tv =>
        fullBounds(tv.origin)
          .ensuring(containsNoInternalTypes(_))
    }

  override def bounds(sym: Symbol)(implicit ctx: Context): TypeBounds = {
    mapping(sym) match {
      case null => null
      case tv =>
        def retrieveBounds: TypeBounds =
          bounds(tv.origin) match {
            case TypeAlias(tpr: TypeParamRef) if reverseMapping.contains(tpr) =>
              TypeAlias(reverseMapping(tpr).typeRef)
            case tb => tb
          }
        retrieveBounds
          //.reporting({ res => i"gadt bounds $sym: $res" }, gadts)
          //.ensuring(containsNoInternalTypes(_))
    }
  }

  override def contains(sym: Symbol)(implicit ctx: Context): Boolean = mapping(sym) ne null

  override def approximation(sym: Symbol, fromBelow: Boolean)(implicit ctx: Context): Type = {
    val res = approximation(tvarOrError(sym).origin, fromBelow = fromBelow)
    gadts.println(i"approximating $sym ~> $res")
    res
  }

  override def fresh: GadtConstraint = new ProperGadtConstraint(
    myConstraint,
    mapping,
    reverseMapping
  )

  def restore(other: GadtConstraint): Unit = other match {
    case other: ProperGadtConstraint =>
      this.myConstraint = other.myConstraint
      this.mapping = other.mapping
      this.reverseMapping = other.reverseMapping
    case _ => ;
  }

  override def isEmpty: Boolean = mapping.size == 0

  // ---- Protected/internal -----------------------------------------------

  implicit override def ctx(implicit ctx: Context): Context = ctx

  override protected def constraint = myConstraint
  override protected def constraint_=(c: Constraint) = myConstraint = c

  override def isSubType(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = ctx.typeComparer.isSubType(tp1, tp2)
  override def isSameType(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = ctx.typeComparer.isSameType(tp1, tp2)

   override def nonParamBounds(param: TypeParamRef)(implicit ctx: Context): TypeBounds =
     constraint.nonParamBounds(param) match {
       case TypeAlias(tpr: TypeParamRef) => TypeAlias(externalize(tpr))
       case tb => tb
     }

   override def fullLowerBound(param: TypeParamRef)(implicit ctx: Context): Type =
     (nonParamBounds(param).lo /: constraint.minLower(param)) {
       (t, u) => t | externalize(u)
     }

   override def fullUpperBound(param: TypeParamRef)(implicit ctx: Context): Type =
     (nonParamBounds(param).hi /: constraint.minUpper(param)) {
       (t, u) => t & externalize(u)
     }

  // ---- Private ----------------------------------------------------------

  private[this] def externalize(param: TypeParamRef)(implicit ctx: Context): Type =
    reverseMapping(param) match {
      case sym: Symbol => sym.typeRef
      case null => param
    }

  private[this] def tvarOrError(sym: Symbol)(implicit ctx: Context): TypeVar =
    mapping(sym).ensuring(_ ne null, i"not a constrainable symbol: $sym")

  private[this] def containsNoInternalTypes(
    tp: Type,
    acc: TypeAccumulator[Boolean] = null
  )(implicit ctx: Context): Boolean = tp match {
    case tpr: TypeParamRef => !reverseMapping.contains(tpr)
    case tv: TypeVar => !reverseMapping.contains(tv.origin)
    case tp =>
      (if (acc ne null) acc else new ContainsNoInternalTypesAccumulator()).foldOver(true, tp)
  }

  private[this] class ContainsNoInternalTypesAccumulator(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean = x && containsNoInternalTypes(tp)
  }

  // ---- Debug ------------------------------------------------------------

  override def constr_println(msg: => String): Unit = gadtsConstr.println(msg)

  override def toText(printer: Printer): Texts.Text = constraint.toText(printer)

  override def debugBoundsDescription(implicit ctx: Context): String = {
    val sb = new mutable.StringBuilder
    sb ++= constraint.show
    sb += '\n'
    mapping.foreachBinding { case (sym, _) =>
      sb ++= i"$sym: ${fullBounds(sym)}\n"
    }
    sb.result
  }
}

@sharable object EmptyGadtConstraint extends GadtConstraint {
  override def bounds(sym: Symbol)(implicit ctx: Context): TypeBounds = null
  override def fullBounds(sym: Symbol)(implicit ctx: Context): TypeBounds = null

  override def isLess(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean = unsupported("EmptyGadtConstraint.isLess")

  override def isEmpty: Boolean = true

  override def contains(sym: Symbol)(implicit ctx: Context) = false

  override def addToConstraint(params: List[Symbol])(implicit ctx: Context): Boolean = unsupported("EmptyGadtConstraint.addToConstraint")
  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(implicit ctx: Context): Boolean = unsupported("EmptyGadtConstraint.addBound")

  override def approximation(sym: Symbol, fromBelow: Boolean)(implicit ctx: Context): Type = unsupported("EmptyGadtConstraint.approximation")

  override def fresh = new ProperGadtConstraint
  override def restore(other: GadtConstraint): Unit = {
    if (!other.isEmpty) sys.error("cannot restore a non-empty GADTMap")
  }

  override def debugBoundsDescription(implicit ctx: Context): String = "EmptyGadtConstraint"

  override def toText(printer: Printer): Texts.Text = "EmptyGadtConstraint"
}
