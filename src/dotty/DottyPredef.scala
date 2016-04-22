package dotty

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import scala.collection.mutable.{ArrayOps, WrappedArray}
import dotty.runtime.vc._
import scala.Predef.???
import scala.collection.Seq

/** unimplemented implicit for TypeTag */
object DottyPredef {
  implicit def typeTag[T]: TypeTag[T] = ???

  implicit def arrayTag[T](implicit ctag: ClassTag[T]): ClassTag[Array[T]] =
    ctag.wrap

  /** A fall-back implicit to compare values of any types.
   *  The compiler will restrict implicit instances of `eqAny`. An instance
   *  `eqAny[T, U]` is _valid_ if `T <: U` or `U <: T` or both `T` and `U` are
   *  Eq-free. A type `S` is Eq-free if there is no implicit instance of `Eq[S, S]`.
   *  An implicit search will fail instead of returning an invalid `eqAny` instance.
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq

  implicit def eqNumber   : Eq[Number, Number] = Eq
  implicit def eqString   : Eq[String, String] = Eq

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

  def wrapVCArray[T](xs: Array[T]): WrappedArray[T] =
    new VCWrappedArray[T](xs)

  //def to substitute scala.Predef.genericWrapArray
  def genericWrapArray2[T](xs: Array[T]): WrappedArray[T] = {
    xs match {
      case x: Array[T] if x.isInstanceOf[VCArrayPrototype[_]] => wrapVCArray(xs)
      //impl of scala.Predef.genericWrapArray(x)
      case x: Array[T] =>
        if (xs eq null) null
        else WrappedArray.make(xs)
      case _ => ???
    }
  }

  def refVCArray[T /*<: AnyVal*/](xs: Array[T]): ArrayOps[T] =
    new VCArrayOps[T](xs)

  //def to substitute scala.Predef.genericArrayOps
  def genericArrayOps2[T](xs: Array[T]): ArrayOps[T] = (xs match {
    case x: Array[T] if x.isInstanceOf[VCArrayPrototype[_]] => refVCArray(x)
    //impl of scala.Predef.genericWrapArray(x)
    case x: Array[AnyRef] => scala.Predef.refArrayOps[AnyRef](x)
    case x: Array[Boolean] => scala.Predef.booleanArrayOps(x)
    case x: Array[Byte] => scala.Predef.byteArrayOps(x)
    case x: Array[Char] => scala.Predef.charArrayOps(x)
    case x: Array[Double] => scala.Predef.doubleArrayOps(x)
    case x: Array[Float] => scala.Predef.floatArrayOps(x)
    case x: Array[Int] => scala.Predef.intArrayOps(x)
    case x: Array[Long] => scala.Predef.longArrayOps(x)
    case x: Array[Short] => scala.Predef.shortArrayOps(x)
    case x: Array[Unit] => scala.Predef.unitArrayOps(x)
    case null => null
  }).asInstanceOf[ArrayOps[T]]
}
