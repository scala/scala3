package scala

/** A marker class indicating that values of kind `T` can be compared. */
class Eq[-T]

/** Besides being a companion object, this object
 *  can also be used as a value that's compatible with
 *  any instance of `Eq`.
 */
object Eq extends Eq[Any] {

  /** An implicit that provides an `Eq` instance for all types `T` that have
   *  a base type `U` such that `U <: EqClass[U]`.
   */
  implicit def eqEq[U, T <: EqClassOf[U] with U]: Eq[T] = Eq
}

