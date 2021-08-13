package dotty.tools
package dotc
package core

import Types._, Contexts._
import printing.Showable

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

  /** Does the constraint's domain contain the type parameters of `tl`? */
  def contains(tl: TypeLambda): Boolean

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

  /** The lower dominator set.
   *
   * This is like `lower`, except that each parameter returned is no smaller than every other returned parameter.
   */
  def minLower(param: TypeParamRef): List[TypeParamRef]

  /** The upper dominator set.
   *
   * This is like `upper`, except that each parameter returned is no greater than every other returned parameter.
   */
  def minUpper(param: TypeParamRef): List[TypeParamRef]

  /** lower(param) \ lower(butNot) */
  def exclusiveLower(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef]

  /** upper(param) \ upper(butNot) */
  def exclusiveUpper(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef]

  /** The constraint bounds for given type parameter `param`.
   *  Poly params that are known to be smaller or greater than `param`
   *  are not contained in the return bounds.
   *  @pre `param` is not part of the constraint domain.
   */
  def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds

  /** A new constraint which is derived from this constraint by adding
   *  entries for all type parameters of `poly`.
   *  @param tvars   A list of type variables associated with the params,
   *                 or Nil if the constraint will just be checked for
   *                 satisfiability but will solved to give instances of
   *                 type variables.
   */
  def add(poly: TypeLambda, tvars: List[TypeVar])(using Context): This

  /** A new constraint which is derived from this constraint by updating
   *  the entry for parameter `param` to `tp`.
   *  `tp` can be one of the following:
   *
   *   - A TypeBounds value, indicating new constraint bounds
   *   - Another type, indicating a solution for the parameter
   *
   * @pre  `this contains param`.
   */
  def updateEntry(param: TypeParamRef, tp: Type)(using Context): This

  /** A constraint that includes the relationship `p1 <: p2`.
   *  `<:` relationships between parameters ("edges") are propagated, but
   *  non-parameter bounds are left alone.
   */
  def addLess(p1: TypeParamRef, p2: TypeParamRef)(using Context): This

  /** A constraint resulting from adding p2 = p1 to this constraint, and at the same
   *  time transferring all bounds of p2 to p1
   */
  def unify(p1: TypeParamRef, p2: TypeParamRef)(using Context): This

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`, or a conservative
   *  approximation of it if that is needed to avoid cycles.
   *  Occurrences nested inside a refinement or prefix are not affected.
   */
  def replace(param: TypeParamRef, tp: Type)(using Context): This

  /** Is entry associated with `tl` removable? This is the case if
   *  all type parameters of the entry are associated with type variables
   *  which have their `inst` fields set.
   */
  def isRemovable(tl: TypeLambda): Boolean

  /** A new constraint with all entries coming from `tl` removed. */
  def remove(tl: TypeLambda)(using Context): This

  /** A new constraint with entry `from` replaced with `to`
   *  Rerences to `from` from within other constraint bounds are updated to `to`.
   *  Type variables are left alone.
   */
  def subst(from: TypeLambda, to: TypeLambda)(using Context): This

  /** Gives for each instantiated type var that does not yet have its `inst` field
   *  set, the instance value stored in the constraint. Storing instances in constraints
   *  is done only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType(tvar: TypeVar): Type

  /** The given `tl` in case it is not contained in this constraint,
   *  a fresh copy of `tl` otherwise.
   */
  def ensureFresh(tl: TypeLambda)(using Context): TypeLambda

  /** The type lambdas constrained by this constraint */
  def domainLambdas: List[TypeLambda]

  /** The type lambda parameters constrained by this constraint */
  def domainParams: List[TypeParamRef]

  /** Check whether predicate holds for all parameters in constraint */
  def forallParams(p: TypeParamRef => Boolean): Boolean

  /** Perform operation `op` on all typevars that do not have their `inst` field set. */
  def foreachTypeVar(op: TypeVar => Unit): Unit

  /** The uninstantiated typevars of this constraint, which still have a bounds constraint
   */
  def uninstVars: collection.Seq[TypeVar]

  /** The weakest constraint that subsumes both this constraint and `other`.
   *  The constraints should be _compatible_, meaning that a type lambda
   *  occurring in both constraints is associated with the same typevars in each.
   *
   *  @param otherHasErrors    If true, handle incompatible constraints by
   *                           returning an approximate constraint, instead of
   *                           failing with an exception
   */
  def & (other: Constraint, otherHasErrors: Boolean)(using Context): Constraint

  /** Whether `tl` is present in both `this` and `that` but is associated with
   *  different TypeVars there, meaning that the constraints cannot be merged.
   */
  def hasConflictingTypeVarsFor(tl: TypeLambda, that: Constraint): Boolean

  /** Check that no constrained parameter contains itself as a bound */
  def checkNonCyclic()(using Context): this.type

  /** Does `param` occur at the toplevel in `tp` ?
   *  Toplevel means: the type itself or a factor in some
   *  combination of `&` or `|` types.
   */
  def occursAtToplevel(param: TypeParamRef, tp: Type)(using Context): Boolean

  /** Check that constraint only refers to TypeParamRefs bound by itself */
  def checkClosed()(using Context): Unit

  /** Check that every typevar om this constraint has as origin a type parameter
   *  of athe type lambda that is associated with the typevar itself.
   */
  def checkConsistentVars()(using Context): Unit
}
