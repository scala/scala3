package dotty.tools
package dotc
package core
package classfile

import java.io.{DataInputStream, InputStream}
import java.nio.{BufferUnderflowException, ByteBuffer}

import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core.Contexts.{ctx, Context}

final class ReusableDataReader() extends DataReader {
  private var data = new Array[Byte](32768)
  private var bb: ByteBuffer = ByteBuffer.wrap(data)
  private var size = 0
  private val reader: DataInputStream = {
    val stream = new InputStream {
      override def read(): Int = try {
        bb.get & 0xff
      } catch {
        case _: BufferUnderflowException => -1
      }

      override def read(b: Array[Byte], off: Int, len: Int): Int = {
        val pos = bb.position()
        bb.get(b, off, len)
        bb.position() - pos
      }

      override def markSupported(): Boolean = false
    }
    new DataInputStream(stream)
  }

  def buf: Array[Byte] = data

  private def nextPositivePowerOfTwo(target: Int): Int = 1 << -Integer.numberOfLeadingZeros(target - 1)

  def reset(file: AbstractFile)(using Context): this.type = {
    this.size = 0
    val bytes = ctx.globalCache.getFileContent(file)
    val size = bytes.length
    if (size > data.length) {
      data = new Array[Byte](nextPositivePowerOfTwo(size))
    } else {
      java.util.Arrays.fill(data, 0.toByte)
    }
    System.arraycopy(bytes, 0, data, 0, size)
    bb = ByteBuffer.wrap(data, 0, size)
    this
  }

  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = bb.get

  def nextBytes(len: Int): Array[Byte] = {
    val result = new Array[Byte](len)
    reader.readFully(result)
    result
  }

  def nextChar: Char = bb.getChar()

  def nextInt: Int = bb.getInt()

  def getChar(mybp: Int): Char = {
    bb.getChar(mybp)
  }

  def getInt(mybp: Int): Int = {
    bb.getInt(mybp)
  }

  def getLong(mybp: Int): Long = {
    bb.getLong(mybp)
  }

  def getFloat(mybp: Int): Float = {
    bb.getFloat(mybp)
  }

  def getDouble(mybp: Int): Double = {
    bb.getDouble(mybp)
  }

  def skip(n: Int): Unit = {
    bb.position(bb.position() + n)
  }
  def bp: Int = bb.position()
  def bp_=(i: Int): Unit = {
    try {
      bb.position(i)
    } catch {
      case ex: IllegalArgumentException =>
        throw ex
    }
  }

  def getByte(mybp: Int): Byte = {
    bb.get(mybp)
  }
  def getBytes(mybp: Int, bytes: Array[Byte]): Unit = {
    val saved = bb.position()
    bb.position(mybp)
    try reader.readFully(bytes)
    finally bb.position(saved)
  }
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
