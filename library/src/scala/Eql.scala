package scala

import annotation.implicitNotFound
import scala.collection.{Seq, Set}

/** A marker trait indicating that values of type `L` can be compared to values of type `R`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait Eql[-L, -R]

/** Companion object containing a few universally known `Eql` instances.
 *  Eql instances involving primitive types or the Null type are handled directly in
 *  the compiler (see Implicits.synthesizedEql), so they are not included here.
 */
object Eql {
  /** A universal `Eql` instance. */
  object derived extends Eql[Any, Any]

  /** A fall-back instance to compare values of any types.
   *  Even though this method is not declared as given, the compiler will
   *  synthesize implicit arguments as solutions to `Eql[T, U]` queries if
   *  the rules of multiversal equality require it.
   */
  def eqlAny[L, R]: Eql[L, R] = derived

  // Instances of `Eql` for common Java types
  given eqlNumber as Eql[Number, Number] = derived
  given eqlString as Eql[String, String] = derived

  // The next three definitions can go into the companion objects of classes
  // Seq and Set. For now they are here in order not to have to touch the
  // source code of these classes
  given eqlSeq[T, U](using eq: Eql[T, U]) as Eql[Seq[T], Seq[U]] = derived
  given eqlSet[T, U](using eq: Eql[T, U]) as Eql[Set[T], Set[U]] = derived
}
