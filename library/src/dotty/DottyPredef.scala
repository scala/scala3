package dotty

import scala.reflect.ClassTag
import scala.Predef.???
import scala.collection.Seq

/** unimplemented implicit for TypeTag */
object DottyPredef {
  /** A fall-back implicit to compare values of any types.
   *  The compiler will restrict implicit instances of `eqAny`. An instance
   *  `eqAny[T, U]` is _valid_ if `T <: U` or `U <: T` or both `T` and `U` are
   *  Eq-free. A type `S` is Eq-free if there is no implicit instance of `Eq[S, S]`.
   *  An implicit search will fail instead of returning an invalid `eqAny` instance.
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq

  implicit def eqNumber   : Eq[Number, Number] = Eq
  implicit def eqString   : Eq[String, String] = Eq
  implicit def eqBoolean  : Eq[Boolean, Boolean] = Eq
  implicit def eqByte     : Eq[Byte, Byte] = Eq
  implicit def eqShort    : Eq[Short, Short] = Eq
  implicit def eqChar     : Eq[Char, Char] = Eq
  implicit def eqInt      : Eq[Int, Int] = Eq
  implicit def eqLong     : Eq[Long, Long] = Eq
  implicit def eqFloat    : Eq[Float, Float] = Eq
  implicit def eqDouble   : Eq[Double, Double] = Eq
  implicit def eqUnit     : Eq[Unit, Unit] = Eq

  // true asymmetry, modeling the (somewhat problematic) nature of equals on Proxies
  implicit def eqProxy    : Eq[Proxy, Any]     = Eq

  implicit def eqSeq[T, U](implicit eq: Eq[T, U]): Eq[Seq[T], Seq[U]] = Eq

  implicit def eqByteNum  : Eq[Byte, Number]   = Eq
  implicit def eqNumByte  : Eq[Number, Byte]   = Eq
  implicit def eqCharNum  : Eq[Char, Number]   = Eq
  implicit def eqNumChar  : Eq[Number, Char]   = Eq
  implicit def eqShortNum : Eq[Short, Number]  = Eq
  implicit def eqNumShort : Eq[Number, Short]  = Eq
  implicit def eqIntNum   : Eq[Int, Number]    = Eq
  implicit def eqNumInt   : Eq[Number, Int]    = Eq
  implicit def eqLongNum  : Eq[Long, Number]   = Eq
  implicit def eqNumLong  : Eq[Number, Long]   = Eq
  implicit def eqFloatNum : Eq[Float, Number]  = Eq
  implicit def eqNumFloat : Eq[Number, Float]  = Eq
  implicit def eqDoubleNum: Eq[Double, Number] = Eq
  implicit def eqNumDouble: Eq[Number, Double] = Eq

  /** A class for implicit values that can serve as implicit conversions
   *  The implicit resolution algorithm will act as if there existed
   *  the additional implicit definition:
   *
   *    def $implicitConversion[T, U](x: T)(c: ImplicitConverter[T, U]): U = c(x)
   *
   *  However, the presence of this definition would slow down implicit search since
   *  its outermost type matches any pair of types. Therefore, implicit search
   *  contains a special case in `Implicits#discardForView` which emulates the
   *  conversion in a more efficient way.
   *
   *  Note that this is a SAM class - function literals are automatically converted
   *  to `ImplicitConverter` values.
   *
   *  Also note that in bootstrapped dotty, `Predef.<:<` should inherit from
   *  `ImplicitConverter`. This would cut the number of special cases in
   *  `discardForView` from two to one.
   */
  abstract class ImplicitConverter[-T, +U] extends Function1[T, U]
}
