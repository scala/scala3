package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._
import collection.mutable
import printing.{Printer, Showable}
import printing.Texts._
import config.Config
import config.Printers.constr

/** Constraint over undetermined type parameters. Constraints are built
 *  over values of the following types:
 *
 *   - TypeLambda    A constraint constrains the type parameters of a set of TypeLambdas
 *   - TypeParamRef  The parameters of the constrained type lambdas
 *   - TypeVar       Every constrained parameter might be associated with a TypeVar
 *                   that has the TypeParamRef as origin.
 */
abstract class Constraint extends Showable {

  type This <: Constraint

  /** Does the constraint's domain contain the type parameters of `pt`? */
  def contains(pt: TypeLambda): Boolean

  /** Does the constraint's domain contain the type parameter `param`? */
  def contains(param: TypeParamRef): Boolean

  /** Does this constraint contain the type variable `tvar` and is it uninstantiated? */
  def contains(tvar: TypeVar): Boolean

  /** The constraint entry for given type parameter `param`, or NoType if `param` is not part of
   *  the constraint domain. Note: Low level, implementation dependent.
   */
  def entry(param: TypeParamRef): Type

  /** The type variable corresponding to parameter `param`, or
   *  NoType, if `param` is not in constrained or is not paired with a type variable.
   */
  def typeVarOfParam(param: TypeParamRef): Type

  /** Is it known that `param1 <:< param2`? */
  def isLess(param1: TypeParamRef, param2: TypeParamRef): Boolean

  /** The parameters that are known to be smaller wrt <: than `param` */
  def lower(param: TypeParamRef): List[TypeParamRef]

  /** The parameters that are known to be greater wrt <: than `param` */
  def upper(param: TypeParamRef): List[TypeParamRef]

  /** lower(param) \ lower(butNot) */
  def exclusiveLower(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef]

  /** upper(param) \ upper(butNot) */
  def exclusiveUpper(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef]

  /** The constraint bounds for given type parameter `param`.
   *  Poly params that are known to be smaller or greater than `param`
   *  are not contained in the return bounds.
   *  @pre `param` is not part of the constraint domain.
   */
  def nonParamBounds(param: TypeParamRef): TypeBounds

  /** The lower bound of `param` including all known-to-be-smaller parameters */
  def fullLowerBound(param: TypeParamRef)(implicit ctx: Context): Type

  /** The upper bound of `param` including all known-to-be-greater parameters */
  def fullUpperBound(param: TypeParamRef)(implicit ctx: Context): Type

  /** The bounds of `param` including all known-to-be-smaller and -greater parameters */
  def fullBounds(param: TypeParamRef)(implicit ctx: Context): TypeBounds

  /** A new constraint which is derived from this constraint by adding
   *  entries for all type parameters of `poly`.
   *  @param tvars   A list of type variables associated with the params,
   *                 or Nil if the constraint will just be checked for
   *                 satisfiability but will solved to give instances of
   *                 type variables.
   */
  def add(poly: TypeLambda, tvars: List[TypeVar])(implicit ctx: Context): This

  /** A new constraint which is derived from this constraint by updating
   *  the entry for parameter `param` to `tp`.
   *  `tp` can be one of the following:
   *
   *   - A TypeBounds value, indicating new constraint bounds
   *   - Another type, indicating a solution for the parameter
   *
   * @pre  `this contains param`.
   */
  def updateEntry(param: TypeParamRef, tp: Type)(implicit ctx: Context): This

  /** A constraint that includes the relationship `p1 <: p2`.
   *  `<:` relationships between parameters ("edges") are propagated, but
   *  non-parameter bounds are left alone.
   */
  def addLess(p1: TypeParamRef, p2: TypeParamRef)(implicit ctx: Context): This

  /** A constraint resulting from adding p2 = p1 to this constraint, and at the same
   *  time transferring all bounds of p2 to p1
   */
  def unify(p1: TypeParamRef, p2: TypeParamRef)(implicit ctx: Context): This

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`, or a conservative
   *  approximation of it if that is needed to avoid cycles.
   *  Occurrences nested inside a refinement or prefix are not affected.
   */
  def replace(param: TypeParamRef, tp: Type)(implicit ctx: Context): This

  /** Is entry associated with `pt` removable? This is the case if
   *  all type parameters of the entry are associated with type variables
   *  which have their `inst` fields set.
   */
  def isRemovable(pt: TypeLambda): Boolean

  /** A new constraint with all entries coming from `pt` removed. */
  def remove(pt: TypeLambda)(implicit ctx: Context): This

  /** The type lambdas constrained by this constraint */
  def domainLambdas: List[TypeLambda]

  /** The type lambda parameters constrained by this constraint */
  def domainParams: List[TypeParamRef]

  /** Check whether predicate holds for all parameters in constraint */
  def forallParams(p: TypeParamRef => Boolean): Boolean

  /** Perform operation `op` on all typevars, or only on uninstantiated
   *  typevars, depending on whether `uninstOnly` is set or not.
   */
  def foreachTypeVar(op: TypeVar => Unit): Unit

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar]

  /** The weakest constraint that subsumes both this constraint and `other` */
  def & (other: Constraint)(implicit ctx: Context): Constraint

  /** Check that no constrained parameter contains itself as a bound */
  def checkNonCyclic()(implicit ctx: Context): Unit

  /** Check that constraint only refers to TypeParamRefs bound by itself */
  def checkClosed()(implicit ctx: Context): Unit
}
