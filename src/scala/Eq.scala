package scala

import annotation.implicitNotFound

/** A marker trait indicating that values of kind `T` can be compared to values of type `U`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
trait Eq[-L, -R]

/** Besides being a companion object, this object
 *  can also be used as a value that's compatible with
 *  any instance of `Eq`.
 */
object Eq extends Eq[Any, Any] {

  /** A fall-back implicit to compare values of any types.
   *  The compiler will restrict implicit instances of `eqAny`. An instance
   *  `eqAny[T, U]` is _invalid_ if `T` or `U` is a non-bottom type that
   *  has an implicit `Eq[T, T]` (respectively `Eq[U, U]`) instance.
   *  An implicit search will fail instead of returning an invalid `eqAny` instance,
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq
}

