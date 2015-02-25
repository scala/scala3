package dotty.tools
package dotc
package core
package pickling


import TastyBuffer._
import TastyName.NameRef
import collection.mutable

/** A byte array bufferfer that can be filled with bytes or natural numbers in TASTY format,
 *  and that supports reading and patching addresses represented as natural numbers.
 */
class TastyReader(val bytes: Array[Byte], val from: Addr, val end: Addr) {
  
  def this(bytes: Array[Byte]) = this(bytes, Addr(0), Addr(bytes.length))
  
  private var bp: Int = from.index
  
  def currentAddr: Addr = Addr(bp)
    
  def atEnd: Boolean = bp == end.index
  
  /** Read a byte of data. */
  def readByte(): Int = {
    val result = bytes(bp) & 0xff
    bp += 1
    result
  }
  
  /** Read the next `n` bytes of `data`. */
  def readBytes(n: Int): Array[Byte] = {
    val result = new Array[Byte](n)
    Array.copy(bytes, bp, result, 0, n)
    bp += n
    result
  }

  /** Read a natural number fitting in an Int in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def readNat(): Int = readLongNat.toInt
    
  /** Read a natural number fitting in a Long in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = bytes(bp)
      x = (x << 7) | (b & 0x7f)
      bp += 1
    } while ((b & 0x80) == 0)
    x
  }
  
  /** Read `nbytes` bytes in big endian format into a Long */
  def readRaw(nbytes: Int): Unit = {
    def recur(x: Long, n: Int): Long = 
      if (n == 0) x else recur((x << 8) | (readByte & 0xff), n - 1)
    recur(0, nbytes)
  }
  
  def readNameRef() = NameRef(readNat())
  
  def readEnd(): Addr = Addr(readNat() + bp)
  
  def skipTo(addr: Addr): Unit = 
    bp = addr.index
  
  def until[T](end: Addr)(op: => T): List[T] = {
    val buf = new mutable.ListBuffer[T]
    while (bp < end.index) buf += op
    assert(bp == end.index)
    buf.toList
  }
}
