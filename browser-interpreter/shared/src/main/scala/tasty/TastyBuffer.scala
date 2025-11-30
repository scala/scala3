package tasty

/**
 * Cross-platform TASTy buffer types.
 * Adapted from dotty.tools.tasty.TastyBuffer for Scala.js compatibility.
 */
object TastyBuffer {

  /** The number of digits of the natural number `nat`, written in base 128 format. */
  def natSize(nat: Int): Int = {
    def loop(n: Int, acc: Int): Int =
      if (n < 128) acc else loop(n >>> 7, acc + 1)
    loop(nat, 1)
  }

  /** An address pointing to an index in a Tasty buffer's byte array */
  case class Addr(index: Int) extends AnyVal {
    def - (delta: Int): Addr = Addr(this.index - delta)
    def + (delta: Int): Addr = Addr(this.index + delta)

    def relativeTo(base: Addr): Addr = this - base.index - AddrWidth

    def ==(that: Addr): Boolean = this.index == that.index
    def !=(that: Addr): Boolean = this.index != that.index
  }

  val NoAddr: Addr = Addr(-1)

  /** The maximal number of address bytes.
   *  Since addresses are written as base-128 natural numbers,
   *  the value of 4 gives a maximal array size of 256M.
   */
  final val AddrWidth = 4

  /** An address referring to a serialized name */
  case class NameRef(index: Int) extends AnyVal
}

