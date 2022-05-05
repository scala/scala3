package scala

import annotation.implicitNotFound
import scala.collection.{Seq, Set}

/** A marker trait indicating that values of type `L` can be compared to values of type `R`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait CanEqual[-L, -R]

/** Companion object containing a few universally known `CanEqual` instances.
 *  CanEqual instances involving primitive types or the Null type are handled directly in
 *  the compiler (see Implicits.synthesizedCanEqual), so they are not included here.
 */
object CanEqual {
  /** A universal `CanEqual` instance. */
  object derived extends CanEqual[Any, Any]

  /** A fall-back instance to compare values of any types.
   *  Even though this method is not declared as given, the compiler will
   *  synthesize implicit arguments as solutions to `CanEqual[T, U]` queries if
   *  the rules of multiversal equality require it.
   */
  def canEqualAny[L, R]: CanEqual[L, R] = derived

  // Instances of `CanEqual` for common Java types
  given canEqualNumber: CanEqual[Number, Number] = derived
  given canEqualString: CanEqual[String, String] = derived

  // The next 6 definitions can go into the companion objects of their corresponding
  // classes. For now they are here in order not to have to touch the
  // source code of these classes
  given canEqualSeqs[T, U](using eq: CanEqual[T, U]): CanEqual[Seq[T], Seq[U]] = derived
  given canEqualSeq[T](using eq: CanEqual[T, T]): CanEqual[Seq[T], Seq[T]] = derived // for `case Nil` in pattern matching

  given canEqualSet[T, U](using eq: CanEqual[T, U]): CanEqual[Set[T], Set[U]] = derived

  given canEqualOptions[T, U](using eq: CanEqual[T, U]): CanEqual[Option[T], Option[U]] = derived
  given canEqualOption[T](using eq: CanEqual[T, T]): CanEqual[Option[T], Option[T]] = derived // for `case None` in pattern matching

  given canEqualEither[L1, R1, L2, R2](
    using eqL: CanEqual[L1, L2], eqR: CanEqual[R1, R2]
  ): CanEqual[Either[L1, R1], Either[L2, R2]] = derived
}
