package dotty.tools
package dotc
package core
package classfile

import java.io.{DataInputStream, InputStream}
import java.nio.ByteBuffer

final class DataReader(file: dotty.tools.io.AbstractFile) {
  private val bb: ByteBuffer = ByteBuffer.wrap(file.toByteArray)
  private val reader: DataInputStream = {
    val stream = new InputStream {
      override def read(): Int =
        bb.get & 0xff
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        bb.get(b, off, len)
        len // ByteBuffer will throw if `len` bytes are not available
    }
    new DataInputStream(stream)
  }

  def nextByte: Byte = bb.get

  def nextBytes(len: Int): Array[Byte] = {
    val result = new Array[Byte](len)
    bb.get(result, 0, result.length)
    result
  }

  def nextChar: Char = bb.getChar()
  def nextInt: Int = bb.getInt()

  def getChar(mybp: Int): Char = bb.getChar(mybp)
  def getInt(mybp: Int): Int = bb.getInt(mybp)
  def getLong(mybp: Int): Long = bb.getLong(mybp)
  def getFloat(mybp: Int): Float = bb.getFloat(mybp)
  def getDouble(mybp: Int): Double = bb.getDouble(mybp)

  def skip(n: Int): Unit = bb.position(bb.position() + n)

  def bp: Int = bb.position()
  def bp_=(i: Int): Unit = bb.position(i)

  def getByte(mybp: Int): Byte = bb.get(mybp)
  def getBytes(mybp: Int, bytes: Array[Byte]): Unit = bb.get(mybp, bytes, 0, bytes.length)

  def getUTF(mybp: Int, len: Int): String = {
    val saved = bb.position()
    val savedLimit = bb.limit()
    bb.position(mybp)
    bb.limit(mybp + len)
    try reader.readUTF()
    finally {
      bb.limit(savedLimit)
      bb.position(saved)
    }
  }
}
