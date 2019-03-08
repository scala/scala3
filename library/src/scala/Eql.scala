package scala

import annotation.implicitNotFound
import scala.collection.{GenSeq, Set}

/** A marker trait indicating that values of type `L` can be compared to values of type `R`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait Eql[-L, -R]

/** Companion object containing a few universally known `Eql` instances.
 *  Eql instances involving primitive types or the Null type are handled directly in
 *  the compiler (see Implicits.synthesizedEq), so they are not included here.
 */
object Eql {
  /** A non-implied universal `Eql` instance. */
  object derived extends Eql[Any, Any]

  /** A fall-back instance to compare values of any types.
   *  Even though this method is not declared implied, the compiler will
   *  compute implied instances as solutions to `Eql[T, U]` queries if
   *  the rules of multiversal equality require it.
   */
  def eqlAny[L, R]: Eql[L, R] = derived

  // Instances of `Eql` for common Java types
  implicit def eqlNumber   : Eql[Number, Number] = derived
  implicit def eqlString   : Eql[String, String] = derived

  // The next three definitions can go into the companion objects of classes
  // Seq, Set, and Proxy. For now they are here in order not to have to touch the
  // source code of these classes
  implicit def eqlSeq[T, U](implicit eq: Eql[T, U]): Eql[GenSeq[T], GenSeq[U]] = derived
  implicit def eqlSet[T, U](implicit eq: Eql[T, U]): Eql[Set[T], Set[U]] = derived

  // true asymmetry, modeling the (somewhat problematic) nature of equals on Proxies
  implicit def eqlProxy    : Eql[Proxy, AnyRef]  = derived
}
