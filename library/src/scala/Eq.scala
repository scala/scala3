package scala

import annotation.implicitNotFound
import scala.collection.{GenSeq, Set}

/** A marker trait indicating that values of type `L` can be compared to values of type `R`. */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait Eq[-L, -R]

/** Besides being a companion object, this object
 *  can also be used as a value that's compatible with
 *  any instance of `Eq`.
 */
object Eq {
  object derived extends Eq[Any, Any]

  /** A fall-back "implicit" to compare values of any types.
   *  Even though this method is not declared implicit, the compiler will
   *  compute instances as solutions to `Eq[T, U]` queries if `T <: U` or `U <: T`
   *  or both `T` and `U` are Eq-free. A type `S` is Eq-free if there is no
   *  implicit instance of type `Eq[S, S]`.
   */
  def eqAny[L, R]: Eq[L, R] = derived

  // Instances of `Eq` for common types

  implicit def eqNumber   : Eq[Number, Number] = derived
  implicit def eqString   : Eq[String, String] = derived
  implicit def eqBoolean  : Eq[Boolean, Boolean] = derived
  implicit def eqByte     : Eq[Byte, Byte] = derived
  implicit def eqShort    : Eq[Short, Short] = derived
  implicit def eqChar     : Eq[Char, Char] = derived
  implicit def eqInt      : Eq[Int, Int] = derived
  implicit def eqLong     : Eq[Long, Long] = derived
  implicit def eqFloat    : Eq[Float, Float] = derived
  implicit def eqDouble   : Eq[Double, Double] = derived
  implicit def eqUnit     : Eq[Unit, Unit] = derived

  // true asymmetry, modeling the (somewhat problematic) nature of equals on Proxies
  implicit def eqProxy    : Eq[Proxy, Any]     = derived

  implicit def eqSeq[T, U](implicit eq: Eq[T, U]): Eq[GenSeq[T], GenSeq[U]] = derived
  implicit def eqSet[T, U](implicit eq: Eq[T, U]): Eq[Set[T], Set[U]] = derived

  implicit def eqByteNum  : Eq[Byte, Number]   = derived
  implicit def eqNumByte  : Eq[Number, Byte]   = derived
  implicit def eqCharNum  : Eq[Char, Number]   = derived
  implicit def eqNumChar  : Eq[Number, Char]   = derived
  implicit def eqShortNum : Eq[Short, Number]  = derived
  implicit def eqNumShort : Eq[Number, Short]  = derived
  implicit def eqIntNum   : Eq[Int, Number]    = derived
  implicit def eqNumInt   : Eq[Number, Int]    = derived
  implicit def eqLongNum  : Eq[Long, Number]   = derived
  implicit def eqNumLong  : Eq[Number, Long]   = derived
  implicit def eqFloatNum : Eq[Float, Number]  = derived
  implicit def eqNumFloat : Eq[Number, Float]  = derived
  implicit def eqDoubleNum: Eq[Double, Number] = derived
  implicit def eqNumDouble: Eq[Number, Double] = derived

  implicit def eqSBoolJBool: Eq[Boolean, java.lang.Boolean] = derived
  implicit def eqJBoolSBool: Eq[java.lang.Boolean, Boolean] = derived
}
