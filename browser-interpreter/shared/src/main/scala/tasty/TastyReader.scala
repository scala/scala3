package tasty

import scala.collection.mutable
import TastyBuffer._

/**
 * Cross-platform TASTy reader.
 * Adapted from dotty.tools.tasty.TastyReader for Scala.js compatibility.
 *
 * A byte array buffer that can be filled with bytes or natural numbers in TASTY format,
 * and that supports reading and patching addresses represented as natural numbers.
 *
 * @param bytes    The array containing data
 * @param start    The position from which to read
 * @param end      The position one greater than the last byte to be read
 * @param base     The index referenced by the logical zero address Addr(0)
 */
class TastyReader(val bytes: Array[Byte], start: Int, end: Int, val base: Int = 0) {

  def this(bytes: Array[Byte]) = this(bytes, 0, bytes.length)

  private var bp: Int = start

  def addr(idx: Int): Addr = Addr(idx - base)
  def index(addr: Addr): Int = addr.index + base

  /** The address of the first byte to read, respectively byte that was read */
  def startAddr: Addr = addr(start)

  /** The address of the next byte to read */
  def currentAddr: Addr = addr(bp)

  /** the address one greater than the last byte to read */
  def endAddr: Addr = addr(end)

  /** Have all bytes been read? */
  def isAtEnd: Boolean = bp == end

  /** A new reader over the same array with the same address base, but with
   *  specified start and end positions
   */
  def subReader(start: Addr, end: Addr): TastyReader =
    new TastyReader(bytes, index(start), index(end), base)

  /** Read a byte of data. */
  def readByte(): Int = {
    val result = bytes(bp) & 0xff
    bp += 1
    result
  }

  /** Returns the next byte of data as a natural number without advancing the read position */
  def nextByte: Int = bytes(bp) & 0xff

  /** Read the next `n` bytes of `data`. */
  def readBytes(n: Int): Array[Byte] = {
    val result = new Array[Byte](n)
    // Cross-platform arraycopy
    var i = 0
    while (i < n) {
      result(i) = bytes(bp + i)
      i += 1
    }
    bp += n
    result
  }

  /** Read a natural number fitting in an Int in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def readNat(): Int = readLongNat().toInt

  /** Read an integer number in 2's complement big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def readInt(): Int = readLongInt().toInt

  /** Read a natural number fitting in a Long in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    var continue = true
    while (continue) {
      b = bytes(bp)
      x = (x << 7) | (b & 0x7f)
      bp += 1
      continue = (b & 0x80) == 0
    }
    x
  }

  /** Read a long integer number in 2's complement big endian format, base 128. */
  def readLongInt(): Long = {
    var b = bytes(bp)
    var x: Long = (b << 1).toByte >> 1 // sign extend with bit 6.
    bp += 1
    while ((b & 0x80) == 0) {
      b = bytes(bp)
      x = (x << 7) | (b & 0x7f)
      bp += 1
    }
    x
  }

  /** Read an uncompressed Long stored in 8 bytes in big endian format */
  def readUncompressedLong(): Long = {
    var x: Long = 0
    var i = 0
    while (i < 8) {
      x = (x << 8) | (readByte() & 0xff)
      i += 1
    }
    x
  }

  /** Read a UTF8 string encoded as `Nat UTF8-CodePoint*`,
   *  where the `Nat` is the length of the code-points bytes.
   *
   *  Cross-platform UTF-8 decoding.
   */
  def readUtf8(): String = {
    val length = readNat()
    if (length == 0) ""
    else {
      val utf8Bytes = readBytes(length)
      decodeUtf8(utf8Bytes)
    }
  }

  /** Cross-platform UTF-8 decoding */
  private def decodeUtf8(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    var i = 0
    while (i < bytes.length) {
      val b = bytes(i) & 0xff
      if ((b & 0x80) == 0) {
        // Single byte character (ASCII)
        sb += b.toChar
        i += 1
      } else if ((b & 0xe0) == 0xc0) {
        // Two byte character
        val b2 = bytes(i + 1) & 0x3f
        sb += (((b & 0x1f) << 6) | b2).toChar
        i += 2
      } else if ((b & 0xf0) == 0xe0) {
        // Three byte character
        val b2 = bytes(i + 1) & 0x3f
        val b3 = bytes(i + 2) & 0x3f
        sb += (((b & 0x0f) << 12) | (b2 << 6) | b3).toChar
        i += 3
      } else if ((b & 0xf8) == 0xf0) {
        // Four byte character (supplementary plane)
        val b2 = bytes(i + 1) & 0x3f
        val b3 = bytes(i + 2) & 0x3f
        val b4 = bytes(i + 3) & 0x3f
        val codePoint = ((b & 0x07) << 18) | (b2 << 12) | (b3 << 6) | b4
        // Convert to surrogate pair
        val high = ((codePoint - 0x10000) >> 10) + 0xD800
        val low = ((codePoint - 0x10000) & 0x3FF) + 0xDC00
        sb += high.toChar
        sb += low.toChar
        i += 4
      } else {
        // Invalid UTF-8, skip
        sb += '?'
        i += 1
      }
    }
    sb.toString
  }

  /** Read a natural number and return as a NameRef */
  def readNameRef(): NameRef = NameRef(readNat())

  /** Read a natural number and return as an address */
  def readAddr(): Addr = Addr(readNat())

  /** Read a length number and return the absolute end address implied by it,
   *  given as <address following length field> + <length-value-read>.
   */
  def readEnd(): Addr = addr(readNat() + bp)

  /** Set read position to the one pointed to by `addr` */
  def goto(addr: Addr): Unit =
    bp = index(addr)

  /** Perform `op` until `end` address is reached and collect results in a list. */
  def until[T](end: Addr)(op: => T): List[T] = {
    val buf = new mutable.ListBuffer[T]
    while (bp < index(end)) buf += op
    assert(bp == index(end))
    buf.toList
  }

  /** If before given `end` address, the result of `op`, otherwise `default` */
  def ifBefore[T](end: Addr)(op: => T, default: T): T =
    if (bp < index(end)) op else default

  /** Perform `op` while condition `cond` holds and collect results in a list. */
  def collectWhile[T](cond: => Boolean)(op: => T): List[T] = {
    val buf = new mutable.ListBuffer[T]
    while (cond) buf += op
    buf.toList
  }
}

