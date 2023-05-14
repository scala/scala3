package dotty.tools
package dotc
package core
package classfile

import scala.language.unsafeNulls

import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble
import java.io.{ByteArrayInputStream, DataInputStream}

import io.AbstractFile

/**
 * This class reads files byte per byte. Only used by ClassFileParser
 *
 * @author Philippe Altherr
 * @version 1.0, 23/03/2004
 */
final class AbstractFileReader(val buf: Array[Byte]) extends DataReader {
  def this(file: AbstractFile) = this(file.toByteArray)

  /** the current input pointer
   */
  var bp: Int = 0

  /** extract a byte at position bp from buf
   */
  def getByte(mybp: Int): Byte =
    buf(mybp)

  def getBytes(mybp: Int, bytes: Array[Byte]): Unit = {
    System.arraycopy(buf, mybp, bytes, 0, bytes.length)
  }

  /** return byte at offset 'pos'
   */
  @throws(classOf[IndexOutOfBoundsException])
  def byteAt(pos: Int): Byte = buf(pos)

  /** read a byte
   */
  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = {
    val b = buf(bp)
    bp += 1
    b
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = {
    bp += len
    buf.slice(bp - len, bp)
  }

  /** read a character
   */
  def nextChar: Char =
    (((nextByte & 0xff) << 8) + (nextByte & 0xff)).toChar

  /** read an integer
   */
  def nextInt: Int =
    ((nextByte & 0xff) << 24) + ((nextByte & 0xff) << 16) +
    ((nextByte & 0xff) <<  8) +  (nextByte & 0xff)


  /** extract a character at position bp from buf
   */
  def getChar(mybp: Int): Char =
    (((getByte(mybp) & 0xff) << 8) + (getByte(mybp+1) & 0xff)).toChar

  /** extract an integer at position bp from buf
   */
  def getInt(mybp: Int): Int =
    ((getByte(mybp) & 0xff) << 24) + ((getByte(mybp + 1) & 0xff) << 16) +
    ((getByte(mybp + 2) & 0xff) << 8) + (getByte(mybp + 3) & 0xff)

  /** extract a long integer at position bp from buf
   */
  def getLong(mybp: Int): Long =
    (getInt(mybp).toLong << 32) + (getInt(mybp + 4) & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(mybp: Int): Float = intBitsToFloat(getInt(mybp))

  /** extract a double at position bp from buf
   */
  def getDouble(mybp: Int): Double = longBitsToDouble(getLong(mybp))

  def getUTF(mybp: Int, len: Int): String =
    new DataInputStream(new ByteArrayInputStream(buf, mybp, len)).readUTF

  /** skip next 'n' bytes
   */
  def skip(n: Int): Unit = { bp += n }
}

