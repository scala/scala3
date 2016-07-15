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

/** Constraint over undetermined type parameters. Constraints are built
 *  over values of the following types:
 *
 *   - PolyType    A constraint constrains the type parameters of a set of PolyTypes
 *   - PolyParam   The parameters of the constrained polytypes
 *   - TypeVar     Every constrained parameter might be associated with a TypeVar
 *                 that has the PolyParam as origin.
 */
abstract class Constraint extends Showable {

  type This <: Constraint

  /** Does the constraint's domain contain the type parameters of `pt`? */
  def contains(pt: GenericType): Boolean

  /** Does the constraint's domain contain the type parameter `param`? */
  def contains(param: PolyParam): Boolean

  /** Does this constraint contain the type variable `tvar` and is it uninstantiated? */
  def contains(tvar: TypeVar): Boolean

  /** The constraint entry for given type parameter `param`, or NoType if `param` is not part of
   *  the constraint domain.
   */
  def entry(param: PolyParam): Type

  /** The type variable corresponding to parameter `param`, or
   *  NoType, if `param` is not in constrained or is not paired with a type variable.
   */
  def typeVarOfParam(param: PolyParam): Type

  /** Is it known that `param1 <:< param2`? */
  def isLess(param1: PolyParam, param2: PolyParam): Boolean

  /** The parameters that are known to be smaller wrt <: than `param` */
  def lower(param: PolyParam): List[PolyParam]

  /** The parameters that are known to be greater wrt <: than `param` */
  def upper(param: PolyParam): List[PolyParam]

  /** lower(param) \ lower(butNot) */
  def exclusiveLower(param: PolyParam, butNot: PolyParam): List[PolyParam]

  /** upper(param) \ upper(butNot) */
  def exclusiveUpper(param: PolyParam, butNot: PolyParam): List[PolyParam]

  /** The constraint bounds for given type parameter `param`.
   *  Poly params that are known to be smaller or greater than `param`
   *  are not contained in the return bounds.
   *  @pre `param` is not part of the constraint domain.
   */
  def nonParamBounds(param: PolyParam): TypeBounds

  /** The lower bound of `param` including all known-to-be-smaller parameters */
  def fullLowerBound(param: PolyParam)(implicit ctx: Context): Type

  /** The upper bound of `param` including all known-to-be-greater parameters */
  def fullUpperBound(param: PolyParam)(implicit ctx: Context): Type

  /** The bounds of `param` including all known-to-be-smaller and -greater parameters */
  def fullBounds(param: PolyParam)(implicit ctx: Context): TypeBounds

  /** A new constraint which is derived from this constraint by adding
   *  entries for all type parameters of `poly`.
   *  @param tvars   A list of type variables associated with the params,
   *                 or Nil if the constraint will just be checked for
   *                 satisfiability but will solved to give instances of
   *                 type variables.
   */
  def add(poly: GenericType, tvars: List[TypeVar])(implicit ctx: Context): This

  /** A new constraint which is derived from this constraint by updating
   *  the entry for parameter `param` to `tp`.
   *  `tp` can be one of the following:
   *
   *   - A TypeBounds value, indicating new constraint bounds
   *   - Another type, indicating a solution for the parameter
   *
   * @pre  `this contains param`.
   */
  def updateEntry(param: PolyParam, tp: Type)(implicit ctx: Context): This

  /** A constraint that includes the relationship `p1 <: p2`.
   *  `<:` relationships between parameters ("edges") are propagated, but
   *  non-parameter bounds are left alone.
   */
  def addLess(p1: PolyParam, p2: PolyParam)(implicit ctx: Context): This

  /** A constraint resulting from adding p2 = p1 to this constraint, and at the same
   *  time transferring all bounds of p2 to p1
   */
  def unify(p1: PolyParam, p2: PolyParam)(implicit ctx: Context): This

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`, or a conservative
   *  approximation of it if that is needed to avoid cycles.
   *  Occurrences nested inside a refinement or prefix are not affected.
   */
  def replace(param: PolyParam, tp: Type)(implicit ctx: Context): This

  /** Narrow one of the bounds of type parameter `param`
   *  If `isUpper` is true, ensure that `param <: `bound`, otherwise ensure
   *  that `param >: bound`.
   */
  def narrowBound(param: PolyParam, bound: Type, isUpper: Boolean)(implicit ctx: Context): This

  /** Is entry associated with `pt` removable? This is the case if
   *  all type parameters of the entry are associated with type variables
   *  which have their `inst` fields set.
   */
  def isRemovable(pt: GenericType): Boolean

  /** A new constraint with all entries coming from `pt` removed. */
  def remove(pt: GenericType)(implicit ctx: Context): This

  /** The polytypes constrained by this constraint */
  def domainPolys: List[GenericType]

  /** The polytype parameters constrained by this constraint */
  def domainParams: List[PolyParam]

  /** Check whether predicate holds for all parameters in constraint */
  def forallParams(p: PolyParam => Boolean): Boolean

  /** Perform operation `op` on all typevars, or only on uninstantiated
   *  typevars, depending on whether `uninstOnly` is set or not.
   */
  def foreachTypeVar(op: TypeVar => Unit): Unit

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar]

  /** Check that no constrained parameter contains itself as a bound */
  def checkNonCyclic()(implicit ctx: Context): Unit

  /** Check that constraint only refers to PolyParams bound by itself */
  def checkClosed()(implicit ctx: Context): Unit
}
