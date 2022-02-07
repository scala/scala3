package dotty.tools.dotc.util

/** An efficient implementation of sequences of 16 indexed elements with
 *  values 0..15 in a single Long.
 *
 */
class SixteenNibbles(val bits: Long) extends AnyVal {
  import SixteenNibbles._

  def apply(idx: Int): Int =
    (bits >>> (idx * Width)).toInt & Mask

  def updated(idx: Int, value: Int): SixteenNibbles =
    new SixteenNibbles(
      (bits & ~(LongMask << (idx * Width))) |
      ((value & Mask).toLong << (idx * Width)))

  def elements: IndexedSeq[Int] = (0 until 16) map apply

  override def toString: String =
    s"SixteenNibbles(${elements.mkString(", ")})"
}

object SixteenNibbles {
  inline val Width = 4
  inline val Mask = (1 << Width) - 1
  final val LongMask: Long = Mask.toLong
}
