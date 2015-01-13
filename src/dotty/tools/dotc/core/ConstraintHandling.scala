package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._
import Decorators._
import config.Config
import config.Printers._

/** Methods for adding constraints and solving them.
 *
 * Constraints are required to be in normalized form. This means
 * (1) if P <: Q in C then also Q >: P in C
 * (2) if P r Q in C and Q r R in C then also P r R in C, where r is <: or :>
 *
 * "P <: Q in C" means here: There is a constraint P <: H[Q],
 *     where H is the multi-hole context given by:
 *
 *      H = []
 *          H & T
 *          T & H
 *          H | H
 *
 *  (the idea is that a parameter Q in a H context is guaranteed to be a supertype of P).
 *
 * "P >: Q in C" means: There is a constraint P >: L[Q],
 *     where L is the multi-hole context given by:
 *
 *      L = []
 *          L | T
 *          T | L
 *          L & L
 */
trait ConstraintHandling {
  
  implicit val ctx: Context
  
  def isSubType(tp1: Type, tp2: Type): Boolean
  def deSkolemize(tp: Type, toSuper: Boolean): Type
  
  val state: TyperState
  import state.constraint

  /** If the constraint is frozen we cannot add new bounds to the constraint. */
  protected var frozenConstraint = false

  /** If the constraint is ignored, subtype checks only take into account
   *  declared bounds of PolyParams. Used when forming unions and intersectons
   *  of constraint bounds
   */
  private var ignoreConstraint = false

  private def ignoringConstraint[T](op: => T): T = {
    val savedIgnore = ignoreConstraint
    val savedFrozen = frozenConstraint
    ignoreConstraint = true
    frozenConstraint = true
    try op
    finally {
      ignoreConstraint = savedIgnore
      frozenConstraint = savedFrozen
    }
  }

  protected def isSubTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    val saved = frozenConstraint
    frozenConstraint = true
    try isSubType(tp1, tp2)
    finally frozenConstraint = saved
  }

  /** The current bounds of type parameter `param` */
  final def bounds(param: PolyParam): TypeBounds = constraint at param match {
    case bounds: TypeBounds if !ignoreConstraint => bounds
    case _ => param.binder.paramBounds(param.paramNum)
  }

  /** Compare a solution of the constraint instead of the constrained parameters.
   *  The solution maps every parameter to its lower bound.
   */
  protected var solvedConstraint = false

  /** The parameters currently being constrained by addConstraint */
  private var pendingParams: Set[PolyParam] = Set()

  /** Make p2 = p1, transfer all bounds of p2 to p1 */
  private def unify(p1: PolyParam, p2: PolyParam): Boolean = {
    constr.println(s"unifying $p1 $p2")
    val constraint1 = constraint.unify(p1, p2)
    val bounds = constraint1.bounds(p1)
    isSubType(bounds.lo, bounds.hi) && { constraint = constraint1; true }
  }

  /** If current constraint set is not frozen, add the constraint
   *
   *      param >: bound   if fromBelow is true
   *      param <: bound   otherwise
   *
   *  to the bounds of `param`. If `bound` is itself a constrained parameter, also
   *  add the dual constraint to `bound`.
   *  @pre `param` is in the constraint's domain
   *  @return Whether the augmented constraint is still satisfiable.
   */
  def addConstraint(param: PolyParam, bound0: Type, fromBelow: Boolean): Boolean = {

    /** Add bidirectional constraint. If new constraint implies 'A <: B' we also
     *  make sure 'B >: A' gets added and vice versa. Furthermore, if the constraint
     *  implies 'A <: B <: A', A and B get unified.
     */
    def addc(param: PolyParam, bound: Type, fromBelow: Boolean): Boolean = {
      val pbounds0 = constraint.bounds(param)
      bound match {
        case bound: PolyParam if constraint contains bound =>
          val bbounds0 @ TypeBounds(lo, hi) = constraint.bounds(bound)
          if (lo eq hi)
            addc(param, lo, fromBelow)
          else if (param == bound)
            true
          else if (fromBelow && param.occursIn(lo, fromBelow = true))
            unify(param, bound)
          else if (!fromBelow && param.occursIn(hi, fromBelow = false))
            unify(bound, param)
          else {
            val pbounds = prepare(param, bound, fromBelow)
            val bbounds = prepare(bound, param, !fromBelow)
            pbounds.exists && bbounds.exists && {
              install(param, pbounds.bounds, pbounds0)
              install(bound, bbounds.bounds, bbounds0)
              true
            }
          }
        case bound: AndOrType if fromBelow != bound.isAnd =>
          addc(param, bound.tp1, fromBelow) &&
            addc(param, bound.tp2, fromBelow)
        case bound: WildcardType =>
          true
        case bound => // !!! remove to keep the originals
          val pbounds = prepare(param, bound, fromBelow)
          pbounds.exists && {
            install(param, pbounds.bounds, pbounds0)
            true
          }
      }
    }

    /** Install bounds for param */
    def install(param: PolyParam, newBounds: TypeBounds, oldBounds: TypeBounds): Unit = {
      val curBounds = constraint.bounds(param)
      try {
        constraint = constraint.updated(param, newBounds)
      } catch {
        case ex: AssertionError =>
          println(i"error while updating $param $newBounds\n$constraint")
          throw ex
      }
      if (curBounds ne oldBounds) {
        // In this case the bounds were updated previously by a recursive isSubType in
        // the satisfiability check of prepare. Reapply the previously added bounds, but
        // go through a full addConstraint in order to eliminate any cyclic dependencies
        // via unification.
        if (!ignoringConstraint(isSubType(curBounds.lo, newBounds.lo)))
          addConstraint(param, curBounds.lo, fromBelow)
        if (!ignoringConstraint(isSubType(newBounds.hi, curBounds.hi)))
          addConstraint(param, curBounds.hi, !fromBelow)
      }
    }

    /** Compute new bounds for `param` and check whether they are
     *  satisfiable. The check might in turn trigger other additions to the constraint.
     *  @return  The new bounds for `param` (which are not installed yet), or
     *           NoType, if the new constraint would not be satisfiable.
     */
    def prepare(param: PolyParam, bound: Type, fromBelow: Boolean): Type = {
      constr.println(s"prepare ${param.show} ${if (fromBelow) ">:>" else "<:<"} ${bound.show}")
      val oldBounds = constraint.bounds(param)
      val newBounds = ignoringConstraint {
        if (fromBelow) oldBounds.derivedTypeBounds(oldBounds.lo | bound, oldBounds.hi)
        else oldBounds.derivedTypeBounds(oldBounds.lo, oldBounds.hi & bound)
      }
      val ok =
        (param == bound) ||
          (oldBounds eq newBounds) ||
          {
            if (pendingParams contains param) {
              // Why the pendingParams test? It is possible that recursive subtype invocations
              // come back with another constraint for `param`. An example came up when compiling
              // ElimRepeated where we got the constraint
              //
              //      Coll <: IterableLike[Tree, Coll]
              //
              // and added
              //
              //      List[Tree] <: Coll
              //
              // The recursive bounds test is then
              //
              //      List[Tree] <: IterableLike[Tree, Coll]
              //
              // and because of the F-bounded polymorphism in the supertype of List,
              // i.e. List[T] <: IterableLike[T, List[T]], this leads again to
              //
              //      List[Tree] <: Coll
              //
              // If a parameter is already pending, we avoid revisiting it here.
              // Instead we combine the bounds computed here with the originally
              // computed bounds when installing the original type.
              constr.println(i"deferred bounds: $param $newBounds")
              true
            } else {
              pendingParams += param
              try isSubType(newBounds.lo, newBounds.hi)
              finally pendingParams -= param
            }
          }
      if (ok) newBounds else NoType
    }
    val bound = deSkolemize(bound0, toSuper = fromBelow).dealias.stripTypeVar
    def description = s"${param.show} ${if (fromBelow) ">:>" else "<:<"} ${bound.show} to ${constraint.show}"
    constr.println(s"adding $description")
    val res = addc(param, bound, fromBelow)
    constr.println(s"added $description")
    if (Config.checkConstraintsNonCyclicTrans) constraint.checkNonCyclicTrans()
    res
  }

  final def isConstrained(param: PolyParam): Boolean =
    !frozenConstraint && !solvedConstraint && (constraint contains param)

  /** Test whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   */
  final def isSatisfiable: Boolean = {
    val saved = solvedConstraint
    solvedConstraint = true
    try
      constraint.forallParams { param =>
        val TypeBounds(lo, hi) = constraint.at(param)
        isSubType(lo, hi) || {
          ctx.log(i"sub fail $lo <:< $hi")
          ctx.log(i"approximated = ${approxParams(lo)} <:< ${approxParams(hi)}")
          false
        }
      }
    finally solvedConstraint = saved
  }

  /** Solve constraint set for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound. However, any occurrences
   *  of the parameter in a refinement somewhere in the bound are removed.
   *  (Such occurrences can arise for F-bounded types).
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  final def approximation(param: PolyParam, fromBelow: Boolean): Type = {
    val avoidParam = new TypeMap {
      override def stopAtStatic = true
      def apply(tp: Type) = mapOver {
        tp match {
          case tp: RefinedType if param occursIn tp.refinedInfo => tp.parent
          case _ => tp
        }
      }
    }
    val bounds = constraint.bounds(param)
    val bound = if (fromBelow) bounds.lo else bounds.hi
    val inst = avoidParam(bound)
    typr.println(s"approx ${param.show}, from below = $fromBelow, bound = ${bound.show}, inst = ${inst.show}")
    inst
  }

  /** Map that approximates each param in constraint by its lower bound.
   *  Currently only used for diagnostics.
   */
  final def approxParams = new TypeMap { // !!! Dotty problem: Turn this def into a val => -Ycheck:mix fails
    def apply(tp: Type): Type = tp.stripTypeVar match {
      case tp: PolyParam if constraint contains tp =>
        this(constraint.bounds(tp).lo)
      case tp =>
        mapOver(tp)
    }
  }
}
