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
   * Note that underlying operations perform subtype checks - for this reason, recursing on `fullBounds`
   * of some symbol when comparing types might lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(implicit ctx: Context): TypeBounds

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean

  def addEmptyBounds(sym: Symbol)(implicit ctx: Context): Unit
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(implicit ctx: Context): Boolean

  /** Is the symbol registered in the constraint?
   *
   * Note that this is returns `true` even if `sym` is already instantiated to some type,
   * unlike [[Constraint.contains]].
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

  override def addEmptyBounds(sym: Symbol)(implicit ctx: Context): Unit = tvar(sym)

  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(implicit ctx: Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvar(sym)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $sym -> $inst")
        return if (isUpper) isSubType(inst , bound) else isSubType(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: NamedType if contains(nt.symbol) =>
        stripInternalTypeVar(tvar(nt.symbol))
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
          val bound1 = bound.adaptHkVariances(oldUpperBound)
          if (isUpper) addUpperBound(symTvar.origin, bound1)
          else addLowerBound(symTvar.origin, bound1)
      }
      ).reporting({ res =>
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $sym $op $bound = $res\t( $symTvar $op $internalizedBound )"
    }, gadts)
  }

  override def isLess(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean =
    constraint.isLess(tvar(sym1).origin, tvar(sym2).origin)

  override def fullBounds(sym: Symbol)(implicit ctx: Context): TypeBounds =
    mapping(sym) match {
      case null => null
      case tv => fullBounds(tv.origin)
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
        retrieveBounds//.reporting({ res => i"gadt bounds $sym: $res" }, gadts)
    }
  }

  override def contains(sym: Symbol)(implicit ctx: Context): Boolean = mapping(sym) ne null

  override def approximation(sym: Symbol, fromBelow: Boolean)(implicit ctx: Context): Type = {
    val res = approximation(tvar(sym).origin, fromBelow = fromBelow)
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

  override protected def externalize(param: TypeParamRef)(implicit ctx: Context): Type =
    reverseMapping(param) match {
      case sym: Symbol => sym.typeRef
      case null => param
    }

  override def isSubType(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = ctx.typeComparer.isSubType(tp1, tp2)
  override def isSameType(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = ctx.typeComparer.isSameType(tp1, tp2)

  // ---- Private ----------------------------------------------------------

  private[this] def tvar(sym: Symbol)(implicit ctx: Context): TypeVar = {
    mapping(sym) match {
      case tv: TypeVar =>
        tv
      case null =>
        val res = {
          import NameKinds.DepParamName
          // For symbols standing for HK types, we need to preserve the kind information
          // (see also usage of adaptHKvariances above)
          // Ideally we'd always preserve the bounds,
          // but first we need an equivalent of ConstraintHandling#addConstraint
          // TODO: implement the above
          val initialBounds = sym.info match {
            case tb @ TypeBounds(_, hi) if hi.isLambdaSub => tb
            case _ => TypeBounds.empty
          }
          // avoid registering the TypeVar with TyperState / TyperState#constraint
          // - we don't want TyperState instantiating these TypeVars
          // - we don't want TypeComparer constraining these TypeVars
          val poly = PolyType(DepParamName.fresh(sym.name.toTypeName) :: Nil)(
            pt => initialBounds :: Nil,
            pt => defn.AnyType)
          new TypeVar(poly.paramRefs.head, creatorState = null)
        }
        gadts.println(i"GADTMap: created tvar $sym -> $res")
        constraint = constraint.add(res.origin.binder, res :: Nil)
        mapping = mapping.updated(sym, res)
        reverseMapping = reverseMapping.updated(res.origin, sym)
        res
    }
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

  override def addEmptyBounds(sym: Symbol)(implicit ctx: Context): Unit = unsupported("EmptyGadtConstraint.addEmptyBounds")
  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(implicit ctx: Context): Boolean = unsupported("EmptyGadtConstraint.addBound")

  override def approximation(sym: Symbol, fromBelow: Boolean)(implicit ctx: Context): Type = unsupported("EmptyGadtConstraint.approximation")

  override def fresh = new ProperGadtConstraint
  override def restore(other: GadtConstraint): Unit = {
    if (!other.isEmpty) sys.error("cannot restore a non-empty GADTMap")
  }

  override def debugBoundsDescription(implicit ctx: Context): String = "EmptyGadtConstraint"

  override def toText(printer: Printer): Texts.Text = "EmptyGadtConstraint"
}
