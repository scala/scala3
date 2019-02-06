package scala

import annotation.implicitNotFound
import scala.collection.{GenSeq, Set}

/** A marker trait indicating that values of type `L` can be compared to values of type `R`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait Eq[-L, -R]

/** Companion object containing a few universally known `Eql` instances.
 *  Eql instances involving primitive types or the Null type are handled directly in
 *  the compiler (see Implicits.synthesizedEq), so they are not included here.
 */
object Eq {
  /** A non-implied universal `Eql` instance. */
  object derived extends Eq[Any, Any]

  /** A fall-back instance to compare values of any types.
   *  Even though this method is not declared implied, the compiler will
   *  compute implied instances as solutions to `Eql[T, U]` queries if
   *  the rules of multiversal equality require it.
   */
  def eqAny[L, R]: Eq[L, R] = derived

  // Instances of `Eq` for common Java types
  implicit def eqNumber   : Eq[Number, Number] = derived
  implicit def eqString   : Eq[String, String] = derived

  // The next three definitions can go into the companion objects of classes
  // Seq, Set, and Proxy. For now they are here in order not to have to touch the
  // source code of these classes
  implicit def eqSeq[T, U](implicit eq: Eq[T, U]): Eq[GenSeq[T], GenSeq[U]] = derived
  implicit def eqSet[T, U](implicit eq: Eq[T, U]): Eq[Set[T], Set[U]] = derived

  // true asymmetry, modeling the (somewhat problematic) nature of equals on Proxies
  implicit def eqProxy    : Eq[Proxy, AnyRef]  = derived
}
