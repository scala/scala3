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
  def contains(pt: PolyType): Boolean

  /** Does the constraint's domain contain the type parameter `param`? */
  def contains(param: PolyParam): Boolean

  /** Does this constraint contain the type variable `tvar` and is it uninstantiated? */
  def contains(tvar: TypeVar): Boolean
  
  /** The constraint for given type parameter `param`, or NoType if `param` is not part of
   *  the constraint domain.
   */
  def at(param: PolyParam): Type
  
  /** The type variable corresponding to parameter `param`, or
   *  NoType, if `param` is not in constrained or is not paired with a type variable.
   */
  def typeVarOfParam(param: PolyParam): Type
    
  /** The constraint bounds for given type parameter `param`.
   *  @pre `param` is not part of the constraint domain.
   */
  def bounds(param: PolyParam): TypeBounds
  
  /** The non-parameter bounds of this constraint.
   *  Poly params that are `related` are not contained in the return bounds.
   */
  def nonParamBounds(param: PolyParam)(implicit ctx: Context): TypeBounds
  
  /** If `firstIsLower` is `param1 <:< param2`?
   *  Otherwise, is `param2 <:< param1`?
   */
  def related(param1: PolyParam, param2: PolyParam, firstIsLower: Boolean)(implicit ctx: Context): Boolean
  
  /** A new constraint which is derived from this constraint by adding
   *  entries for all type parameters of `poly`.
   *  @param tvars   A list of type variables associated with the params, 
   *                 or Nil if the constraint will just be checked for
   *                 satisfiability but will solved to give instances of
   *                 type variables.
   */
  def add(poly: PolyType, tvars: List[TypeVar])(implicit ctx: Context): This

  /** A new constraint which is derived from this constraint by updating
   *  the entry for parameter `param` to `tpe`.
   *  @pre  `this contains param`.
   *  
   *  `tp` can be one of the following:
   *  
   *   - A TypeBounds value, indicating new constraint bounds
   *   - Another type, indicating a solution for the parameter
   */
  def updated(param: PolyParam, tp: Type)(implicit ctx: Context): This
  
  /** A constraint that includes a the relationship `bound <: param` if `fromBelow` is true
   *  or else `param <: bound` if `fromBelow` is false.
   */
  def order(param: PolyParam, bound: PolyParam, fromBelow: Boolean)(implicit ctx: Context): This

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   *  If `tp` is another polyparam, applies the necessary unifications to avoid a cyclic
   *  constraint.
   */
  def replace(param: PolyParam, tp: Type)(implicit ctx: Context): This
  
  /** Is entry associated with `pt` removable?
   *  @param removedParam The index of a parameter which is still present in the
   *                      entry array, but is going to be removed at the same step,
   *                      or -1 if no such parameter exists.
   */
  def isRemovable(pt: PolyType, removedParam: Int = -1): Boolean
  
  /** A new constraint with all entries coming from `pt` removed. */
  def remove(pt: PolyType)(implicit ctx: Context): This

  /** A constraint resulting by adding p2 = p1 to this constraint, and at the same
   *  time transferring all bounds of p2 to p1
   */
  def unify(p1: PolyParam, p2: PolyParam)(implicit ctx: Context): This

  /** The polytypes constrained by this constraint */
  def domainPolys: List[PolyType]

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

 /** Check that no constrained parameter in `pt` contains itself as a bound */
  def checkNonCyclic(pt: PolyType, entries: Array[Type])(implicit ctx: Context): Unit
  
  /** Check that no constrained parameter contains itself as a bound */
  def checkNonCyclic()(implicit ctx: Context): Unit
  
 /** Check that no constrained parameter contains itself as a bound,
   *  either directly or indirectly. This should be not a stricter criterion
   *  than checkNonCyclic because transitivity should be eliminated always,
   *  but it's good to be paranoid.
   */
  def checkNonCyclicTrans()(implicit ctx: Context): Unit
  
  protected def splitParams(tp: Type, seenFromBelow: Boolean, handleParam: PolyParam => Unit)(implicit ctx: Context): Type = tp match {
    case tp: PolyParam if contains(tp) => 
      handleParam(tp)
      NoType
    case tp: AndOrType if seenFromBelow == tp.isAnd =>
      val tp1 = splitParams(tp.tp1, seenFromBelow, handleParam)
      val tp2 = splitParams(tp.tp2, seenFromBelow, handleParam)
      if (tp1.exists)
        if (tp2.exists) tp.derivedAndOrType(tp1, tp2)
        else tp1
      else if (tp2.exists) tp2
      else NoType
    case _ =>
      tp
  } 
}
