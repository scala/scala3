package dotty.tools
package dotc
package core

import Types.*, Contexts.*
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

  /** The current bounds of type parameter `param` */
  def bounds(param: TypeParamRef)(using Context): TypeBounds

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
   * @pre  `tp` does not contain top-level references to `param`
   *       (see `validBoundsFor`)
   */
  def updateEntry(param: TypeParamRef, tp: Type)(using Context): This

  /** A constraint that includes the relationship `p1 <: p2`.
   *  `<:` relationships between parameters ("edges") are propagated, but
   *  non-parameter bounds are left alone.
   *
   *  @param direction  Must be set to `KeepParam1` or `KeepParam2` when
   *                    `p2 <: p1` is already true depending on which parameter
   *                    the caller intends to keep. This will avoid propagating
   *                    bounds that will be redundant after `p1` and `p2` are
   *                    unified.
   */
  def addLess(p1: TypeParamRef, p2: TypeParamRef,
    direction: UnificationDirection = UnificationDirection.NoUnification)(using Context): This

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

  /** Is `tv` marked as hard in the constraint? */
  def isHard(tv: TypeVar): Boolean

  /** The same as this constraint, but with `tv` marked as hard. */
  def withHard(tv: TypeVar)(using Context): This

  /** Mark toplevel type vars in `tp` as hard. */
  def hardenTypeVars(tp: Type)(using Context): This

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

  /** Whether `tl` is present in both `this` and `that` but is associated with
   *  different TypeVars there, meaning that the constraints cannot be merged.
   */
  def hasConflictingTypeVarsFor(tl: TypeLambda, that: Constraint): Boolean

  /** Does `param` occur at the toplevel in `tp` ?
   *  Toplevel means: the type itself or a factor in some
   *  combination of `&` or `|` types.
   */
  def occursAtToplevel(param: TypeParamRef, tp: Type)(using Context): Boolean

  /** Sanitize `bound` to make it either a valid upper or lower bound for
   *  `param` depending on `isUpper`.
   *
   *  Toplevel references to `param`, are replaced by `Any` if `isUpper` is true
   *  and `Nothing` otherwise.
   *
   *  @see `occursAtTopLevel` for a definition of "toplevel"
   *  @see `validBoundsFor` to sanitize both the lower and upper bound at once.
   */
  def validBoundFor(param: TypeParamRef, bound: Type, isUpper: Boolean)(using Context): Type

  /** Sanitize `bounds` to make them valid constraints for `param`.
   *
   *  @see `validBoundFor` for details.
   */
  def validBoundsFor(param: TypeParamRef, bounds: TypeBounds)(using Context): Type

  /** A string that shows the reverse dependencies maintained by this constraint
   *  (coDeps and contraDeps for OrderingConstraints).
   */
  def depsToString(using Context): String

  /** Does the constraint restricted to variables outside `except` depend on `tv`
   *  in the given direction `co`?
   *  @param `co`  If true, test whether the constraint would change if the variable is made larger
   *               otherwise, test whether the constraint would change if the variable is made smaller.
   */
  def dependsOn(tv: TypeVar, except: TypeVars, co: Boolean)(using Context): Boolean

  /** Depending on Config settngs:
   *   - Under `checkConstraintsNonCyclic`, check that no constrained
   *     parameter contains itself as a bound.
   *   - Under `checkConstraintDeps`, check hat reverse dependencies in
   *     constraints are correct and complete.
   */
  def checkWellFormed()(using Context): this.type

  /** Check that constraint only refers to TypeParamRefs bound by itself */
  def checkClosed()(using Context): Unit

  /** Check that every typevar om this constraint has as origin a type parameter
   *  of athe type lambda that is associated with the typevar itself.
   */
  def checkConsistentVars()(using Context): Unit
}

/** When calling `Constraint#addLess(p1, p2, ...)`, the caller might end up
 *  unifying one parameter with the other, this enum lets `addLess` know which
 *  direction the unification will take.
 */
enum UnificationDirection:
  /** Neither p1 nor p2 will be instantiated. */
  case NoUnification
  /** `p2 := p1`, p1 left uninstantiated. */
  case KeepParam1
  /** `p1 := p2`, p2 left uninstantiated. */
  case KeepParam2
