package scala

import annotation.implicitNotFound

/** A marker class indicating that values of kind `T` can be compared. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
class Eq[-L, -R]

/** Besides being a companion object, this object
 *  can also be used as a value that's compatible with
 *  any instance of `Eq`.
 */
object Eq extends Eq[Any, Any] {

  /** An implicit that provides an `Eq` instance for all types `T`
   *  such that `T <: EqClass[T]`.
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq
}

