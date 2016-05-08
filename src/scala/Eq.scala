package scala

import annotation.implicitNotFound

/** A marker trait indicating that values of kind `L` can be compared to values of type `R`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait Eq[-L, -R]

/** Besides being a companion object, this object
 *  can also be used as a value that's compatible with
 *  any instance of `Eq`.
 */
object Eq extends Eq[Any, Any] {

  /** A fall-back implicit to compare values of any types.
   *  The compiler will restrict implicit instances of `eqAny`. An instance
   *  `eqAny[T, U]` is _valid_ if `T <: U` or `U <: T` or both `T` and `U` are
   *  Eq-free. A type `S` is Eq-free if there is no implicit instance of `Eq[S, S]`.
   *  An implicit search will fail instead of returning an invalid `eqAny` instance.
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq
}

