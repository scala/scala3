package scala.math
package inline

import java.util.Comparator

trait Ordering[T] extends Comparator[T] with PartialOrdering[T] with Serializable:
  outer =>

  inline def tryCompare(x: T, y: T) = Some(compare(x, y))

  def compare(x: T, y: T): Int

  override inline def lteq(x: T, y: T): Boolean = compare(x, y) <= 0
  override inline def gteq(x: T, y: T): Boolean = compare(x, y) >= 0
  override inline def lt(x: T, y: T): Boolean = compare(x, y) < 0
  override inline def gt(x: T, y: T): Boolean = compare(x, y) > 0
  override inline def equiv(x: T, y: T): Boolean = compare(x, y) == 0

  inline def max(x: T, y: T): T = if gteq(x, y) then x else y
  inline def min(x: T, y: T): T = if lteq(x, y) then x else y

  // This is made into a separate trait, because defining the reverse ordering
  // anonymously results in an error:
  //   Implementation restriction: nested inline methods are not supported
  inline def on[U](f: U => T): Ordering[U] = new ReverseOrdering(f) {}

  private trait ReverseOrdering[U](f: U => T) extends Ordering[U]:
    inline def compare(x: U, y: U) = outer.compare(f(x), f(y))

object Ordering:
  trait BigDecimalOrdering extends Ordering[BigDecimal]:
    inline def compare(x: BigDecimal, y: BigDecimal) = x.compare(y)

  trait BigIntOrdering extends Ordering[BigInt]:
    inline def compare(x: BigInt, y: BigInt) = x.compare(y)

  trait ByteOrdering extends Ordering[Byte]:
    inline def compare(x: Byte, y: Byte) = java.lang.Byte.compare(x, y)

  trait CharOrdering extends Ordering[Char]:
    inline def compare(x: Char, y: Char) = java.lang.Character.compare(x, y)

  trait IntOrdering extends Ordering[Int]:
    inline def compare(x: Int, y: Int) = java.lang.Integer.compare(x, y)

  trait LongOrdering extends Ordering[Long]:
    inline def compare(x: Long, y: Long) = java.lang.Long.compare(x, y)

  trait ShortOrdering extends Ordering[Short]:
    inline def compare(x: Short, y: Short) = java.lang.Short.compare(x, y)

  trait FloatIeeeOrdering extends Ordering[Float]:
    inline def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)

  trait DoubleIeeeOrdering extends Ordering[Double]:
    inline def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
