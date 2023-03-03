package dotty.tools.dotc.semanticdb.internal

import scala.language.unsafeNulls

import java.io.IOException
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import SemanticdbOutputStream._

object SemanticdbOutputStream {

  val DEFAULT_BUFFER_SIZE = 4096

  def computePreferredBufferSize(dataLength: Int): Int = {
    if (dataLength > DEFAULT_BUFFER_SIZE) return DEFAULT_BUFFER_SIZE
    dataLength
  }

  def newInstance(output: OutputStream): SemanticdbOutputStream = {
    newInstance(output, DEFAULT_BUFFER_SIZE)
  }

  def newInstance(output: OutputStream, bufferSize: Int): SemanticdbOutputStream = {
    new SemanticdbOutputStream(output, Array.ofDim[Byte](bufferSize))
  }

  def newInstance(byteBuffer: ByteBuffer): SemanticdbOutputStream = {
    newInstance(byteBuffer, DEFAULT_BUFFER_SIZE)
  }

  def newInstance(b: Array[Byte]): SemanticdbOutputStream = {
    new SemanticdbOutputStream(null, b)
  }

  def newInstance(
      byteBuffer: ByteBuffer,
      bufferSize: Int
  ): SemanticdbOutputStream = {
    newInstance(new ByteBufferOutputStream(byteBuffer), bufferSize)
  }

  private class ByteBufferOutputStream(private val byteBuffer: ByteBuffer)
      extends OutputStream {

    override def write(b: Int): Unit = {
      byteBuffer.put(b.toByte)
    }

    override def write(data: Array[Byte], offset: Int, length: Int): Unit = {
      byteBuffer.put(data, offset, length)
    }
  }

  def computeDoubleSize(fieldNumber: Int, value: Double): Int = {
    computeTagSize(fieldNumber) + computeDoubleSizeNoTag(value)
  }

  def computeFloatSize(fieldNumber: Int, value: Float): Int = {
    computeTagSize(fieldNumber) + computeFloatSizeNoTag(value)
  }

  def computeUInt64Size(fieldNumber: Int, value: Long): Int = {
    computeTagSize(fieldNumber) + computeUInt64SizeNoTag(value)
  }

  def computeInt64Size(fieldNumber: Int, value: Long): Int = {
    computeTagSize(fieldNumber) + computeInt64SizeNoTag(value)
  }

  def computeInt32Size(fieldNumber: Int, value: Int): Int = {
    computeTagSize(fieldNumber) + computeInt32SizeNoTag(value)
  }

  def computeFixed64Size(fieldNumber: Int, value: Long): Int = {
    computeTagSize(fieldNumber) + computeFixed64SizeNoTag(value)
  }

  def computeFixed32Size(fieldNumber: Int, value: Int): Int = {
    computeTagSize(fieldNumber) + computeFixed32SizeNoTag(value)
  }

  def computeBoolSize(fieldNumber: Int, value: Boolean): Int = {
    computeTagSize(fieldNumber) + computeBoolSizeNoTag(value)
  }

  def computeStringSize(fieldNumber: Int, value: String): Int = {
    computeTagSize(fieldNumber) + computeStringSizeNoTag(value)
  }

  // def computeBytesSize(fieldNumber: Int, value: ByteString): Int = {
  //   computeTagSize(fieldNumber) + computeBytesSizeNoTag(value)
  // }

  def computeByteArraySize(fieldNumber: Int, value: Array[Byte]): Int = {
    computeTagSize(fieldNumber) + computeByteArraySizeNoTag(value)
  }

  def computeByteBufferSize(fieldNumber: Int, value: ByteBuffer): Int = {
    computeTagSize(fieldNumber) + computeByteBufferSizeNoTag(value)
  }

  def computeUInt32Size(fieldNumber: Int, value: Int): Int = {
    computeTagSize(fieldNumber) + computeUInt32SizeNoTag(value)
  }

  def computeEnumSize(fieldNumber: Int, value: Int): Int = {
    computeTagSize(fieldNumber) + computeEnumSizeNoTag(value)
  }

  def computeSFixed32Size(fieldNumber: Int, value: Int): Int = {
    computeTagSize(fieldNumber) + computeSFixed32SizeNoTag(value)
  }

  def computeSFixed64Size(fieldNumber: Int, value: Long): Int = {
    computeTagSize(fieldNumber) + computeSFixed64SizeNoTag(value)
  }

  def computeSInt32Size(fieldNumber: Int, value: Int): Int = {
    computeTagSize(fieldNumber) + computeSInt32SizeNoTag(value)
  }

  def computeSInt64Size(fieldNumber: Int, value: Long): Int = {
    computeTagSize(fieldNumber) + computeSInt64SizeNoTag(value)
  }

  def computeDoubleSizeNoTag(value: Double): Int = LITTLE_ENDIAN_64_SIZE

  def computeFloatSizeNoTag(value: Float): Int = LITTLE_ENDIAN_32_SIZE

  def computeUInt64SizeNoTag(value: Long): Int = computeRawVarint64Size(value)

  def computeInt64SizeNoTag(value: Long): Int = computeRawVarint64Size(value)

  def computeInt32SizeNoTag(value: Int): Int = {
    if (value >= 0) {
      computeRawVarint32Size(value)
    } else {
      10
    }
  }

  def computeFixed64SizeNoTag(value: Long): Int = LITTLE_ENDIAN_64_SIZE

  def computeFixed32SizeNoTag(value: Int): Int = LITTLE_ENDIAN_32_SIZE

  def computeBoolSizeNoTag(value: Boolean): Int = 1

  def computeStringSizeNoTag(value: String): Int = {
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    computeRawVarint32Size(bytes.length) + bytes.length
  }

  // def computeBytesSizeNoTag(value: ByteString): Int = {
  //   computeRawVarint32Size(value.size) + value.size
  // }

  def computeByteArraySizeNoTag(value: Array[Byte]): Int = {
    computeRawVarint32Size(value.length) + value.length
  }

  def computeByteBufferSizeNoTag(value: ByteBuffer): Int = {
    computeRawVarint32Size(value.capacity()) + value.capacity()
  }

  def computeUInt32SizeNoTag(value: Int): Int = computeRawVarint32Size(value)

  def computeEnumSizeNoTag(value: Int): Int = computeInt32SizeNoTag(value)

  def computeSFixed32SizeNoTag(value: Int): Int = LITTLE_ENDIAN_32_SIZE

  def computeSFixed64SizeNoTag(value: Long): Int = LITTLE_ENDIAN_64_SIZE

  def computeSInt32SizeNoTag(value: Int): Int = {
    computeRawVarint32Size(encodeZigZag32(value))
  }

  def computeSInt64SizeNoTag(value: Long): Int = {
    computeRawVarint64Size(encodeZigZag64(value))
  }

  @SerialVersionUID(-6947486886997889499L)
  class OutOfSpaceException()
      extends IOException(
        "SemanticdbOutputStream was writing to a flat byte array and ran " +
          "out of space."
      )

  def computeTagSize(fieldNumber: Int): Int = {
    computeRawVarint32Size(WireFormat.makeTag(fieldNumber, 0))
  }

  def computeRawVarint32Size(value: Int): Int = {
    if ((value & (0xffffffff << 7)) == 0) return 1
    if ((value & (0xffffffff << 14)) == 0) return 2
    if ((value & (0xffffffff << 21)) == 0) return 3
    if ((value & (0xffffffff << 28)) == 0) return 4
    5
  }

  def computeRawVarint64Size(value: Long): Int = {
    if ((value & (0xFFFFFFFFFFFFFFFFL << 7)) == 0) return 1
    if ((value & (0xFFFFFFFFFFFFFFFFL << 14)) == 0) return 2
    if ((value & (0xFFFFFFFFFFFFFFFFL << 21)) == 0) return 3
    if ((value & (0xFFFFFFFFFFFFFFFFL << 28)) == 0) return 4
    if ((value & (0xFFFFFFFFFFFFFFFFL << 35)) == 0) return 5
    if ((value & (0xFFFFFFFFFFFFFFFFL << 42)) == 0) return 6
    if ((value & (0xFFFFFFFFFFFFFFFFL << 49)) == 0) return 7
    if ((value & (0xFFFFFFFFFFFFFFFFL << 56)) == 0) return 8
    if ((value & (0xFFFFFFFFFFFFFFFFL << 63)) == 0) return 9
    10
  }

  val LITTLE_ENDIAN_32_SIZE = 4

  val LITTLE_ENDIAN_64_SIZE = 8

  def encodeZigZag32(n: Int): Int = (n << 1) ^ (n >> 31)

  def encodeZigZag64(n: Long): Long = (n << 1) ^ (n >> 63)
}

class SemanticdbOutputStream(output: OutputStream, buffer: Array[Byte]) {
  private def refreshBuffer(): Unit = {
    if (output == null) {
      throw new OutOfSpaceException()
    }
    output.write(buffer, 0, position)
    position = 0
  }

  def flush(): Unit = {
    if (output != null) {
      refreshBuffer()
    }
  }

  def spaceLeft(): Int = {
    if (output == null) {
      limit - position
    } else {
      throw new UnsupportedOperationException(
        "spaceLeft() can only be called on SemanticdbOutputStreams that are " +
          "writing to a flat array."
      )
    }
  }

  def checkNoSpaceLeft(): Unit = {
    if (spaceLeft() != 0) {
      throw new IllegalStateException("Did not write as much data as expected.")
    }
  }

  private var position = 0
  private val limit = buffer.length

  private var totalBytesWritten: Int = 0

  def writeDouble(fieldNumber: Int, value: Double): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_FIXED64)
    writeDoubleNoTag(value)
  }

  def writeFloat(fieldNumber: Int, value: Float): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_FIXED32)
    writeFloatNoTag(value)
  }

  def writeUInt64(fieldNumber: Int, value: Long): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeUInt64NoTag(value)
  }

  def writeInt64(fieldNumber: Int, value: Long): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeInt64NoTag(value)
  }

  def writeInt32(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeInt32NoTag(value)
  }

  def writeFixed64(fieldNumber: Int, value: Long): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_FIXED64)
    writeFixed64NoTag(value)
  }

  def writeFixed32(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_FIXED32)
    writeFixed32NoTag(value)
  }

  def writeBool(fieldNumber: Int, value: Boolean): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeBoolNoTag(value)
  }

  def writeString(fieldNumber: Int, value: String): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeStringNoTag(value)
  }

  // def writeBytes(fieldNumber: Int, value: ByteString): Unit = {
  //   writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
  //   writeBytesNoTag(value)
  // }

  def writeByteArray(fieldNumber: Int, value: Array[Byte]): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeByteArrayNoTag(value)
  }

  def writeByteArray(
      fieldNumber: Int,
      value: Array[Byte],
      offset: Int,
      length: Int
  ): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeByteArrayNoTag(value, offset, length)
  }

  def writeByteBuffer(fieldNumber: Int, value: ByteBuffer): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeByteBufferNoTag(value)
  }

  def writeUInt32(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeUInt32NoTag(value)
  }

  def writeEnum(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeEnumNoTag(value)
  }

  def writeSFixed32(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_FIXED32)
    writeSFixed32NoTag(value)
  }

  def writeSFixed64(fieldNumber: Int, value: Long): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_FIXED64)
    writeSFixed64NoTag(value)
  }

  def writeSInt32(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeSInt32NoTag(value)
  }

  def writeSInt64(fieldNumber: Int, value: Long): Unit = {
    writeTag(fieldNumber, WireFormat.WIRETYPE_VARINT)
    writeSInt64NoTag(value)
  }

  def writeDoubleNoTag(value: Double): Unit = {
    writeRawLittleEndian64(java.lang.Double.doubleToLongBits(value))
  }

  def writeFloatNoTag(value: Float): Unit = {
    writeRawLittleEndian32(java.lang.Float.floatToIntBits(value))
  }

  def writeUInt64NoTag(value: Long): Unit = {
    writeRawVarint64(value)
  }

  def writeInt64NoTag(value: Long): Unit = {
    writeRawVarint64(value)
  }

  def writeInt32NoTag(value: Int): Unit = {
    if (value >= 0) {
      writeRawVarint32(value)
    } else {
      writeRawVarint64(value)
    }
  }

  def writeFixed64NoTag(value: Long): Unit = {
    writeRawLittleEndian64(value)
  }

  def writeFixed32NoTag(value: Int): Unit = {
    writeRawLittleEndian32(value)
  }

  def writeBoolNoTag(value: Boolean): Unit = {
    writeRawByte(if (value) 1 else 0)
  }

  def writeStringNoTag(value: String): Unit = {
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    writeRawVarint32(bytes.length)
    writeRawBytes(bytes)
  }

  def writeTag(fieldNumber: Int, wireType: Int): Unit = {
    writeRawVarint32(WireFormat.makeTag(fieldNumber, wireType))
  }

  def writeRawVarint32(value0: Int): Unit = {
    var value = value0
    while (true) {
      if ((value & ~0x7F) == 0) {
        writeRawByte(value)
        return
      } else {
        writeRawByte((value & 0x7F) | 0x80)
        value >>>= 7
      }
    }
  }

  def writeRawVarint64(value0: Long): Unit = {
    var value = value0
    while (true) {
      if ((value & ~0x7FL) == 0) {
        writeRawByte(value.toInt)
        return
      } else {
        writeRawByte((value.toInt & 0x7F) | 0x80)
        value >>>= 7
      }
    }
  }

  def writeRawLittleEndian32(value: Int): Unit = {
    writeRawByte((value) & 0xFF)
    writeRawByte((value >> 8) & 0xFF)
    writeRawByte((value >> 16) & 0xFF)
    writeRawByte((value >> 24) & 0xFF)
  }

  def writeRawLittleEndian64(value: Long): Unit = {
    writeRawByte((value).toInt & 0xFF)
    writeRawByte((value >> 8).toInt & 0xFF)
    writeRawByte((value >> 16).toInt & 0xFF)
    writeRawByte((value >> 24).toInt & 0xFF)
    writeRawByte((value >> 32).toInt & 0xFF)
    writeRawByte((value >> 40).toInt & 0xFF)
    writeRawByte((value >> 48).toInt & 0xFF)
    writeRawByte((value >> 56).toInt & 0xFF)
  }

  // def writeBytesNoTag(value: ByteString): Unit = {
  //   writeRawVarint32(value.size)
  //   writeRawBytes(value)
  // }

  def writeByteArrayNoTag(value: Array[Byte]): Unit = {
    writeRawVarint32(value.length)
    writeRawBytes(value)
  }

  def writeByteArrayNoTag(
      value: Array[Byte],
      offset: Int,
      length: Int
  ): Unit = {
    writeRawVarint32(length)
    writeRawBytes(value, offset, length)
  }

  def writeByteBufferNoTag(value: ByteBuffer): Unit = {
    writeRawVarint32(value.capacity())
    writeRawBytes(value)
  }

  def writeUInt32NoTag(value: Int): Unit = {
    writeRawVarint32(value)
  }

  def writeEnumNoTag(value: Int): Unit = {
    writeInt32NoTag(value)
  }

  def writeSFixed32NoTag(value: Int): Unit = {
    writeRawLittleEndian32(value)
  }

  def writeSFixed64NoTag(value: Long): Unit = {
    writeRawLittleEndian64(value)
  }

  def writeSInt32NoTag(value: Int): Unit = {
    writeRawVarint32(encodeZigZag32(value))
  }

  def writeSInt64NoTag(value: Long): Unit = {
    writeRawVarint64(encodeZigZag64(value))
  }

  def writeRawByte(value: Byte): Unit = {
    if (position == limit) {
      refreshBuffer()
    }
    buffer(position) = value
    position += 1
  }

  def writeRawByte(value: Int): Unit = {
    writeRawByte(value.toByte)
  }

  // def writeRawBytes(value: ByteString): Unit = {
  //   var offset = 0
  //   var length = value.size
  //   if (limit - position >= length) {
  //     // We have room in the current buffer.
  //     value.copyTo(buffer, offset, position, length)
  //     position += length
  //     totalBytesWritten += length
  //   } else {
  //     // Write extends past current buffer.  Fill the rest of this buffer and
  //     // flush.
  //     val bytesWritten = limit - position
  //     value.copyTo(buffer, offset, position, bytesWritten)
  //     offset += bytesWritten
  //     length -= bytesWritten
  //     position = limit
  //     totalBytesWritten += bytesWritten
  //     refreshBuffer()
  //     // Now deal with the rest.
  //     // Since we have an output stream, this is our buffer
  //     // and buffer offset == 0
  //     if (length <= limit) {
  //       value.copyTo(buffer, offset, 0, length)
  //       position = length
  //     } else {
  //       value.slice(offset, offset + length).writeTo(output)
  //     }
  //     totalBytesWritten += length
  //   }
  // }

  def writeRawBytes(value: Array[Byte]): Unit = {
    writeRawBytes(value, 0, value.length)
  }

  def writeRawBytes(value: ByteBuffer): Unit = {
    if (value.hasArray()) {
      writeRawBytes(value.array(), value.arrayOffset(), value.capacity())
    } else {
      val duplicated = value.duplicate()
      duplicated.clear()
      writeRawBytesInternal(duplicated)
    }
  }

  private def writeRawBytesInternal(value: ByteBuffer): Unit = {
    var length = value.remaining()
    if (limit - position >= length) {
      value.get(buffer, position, length)
      position += length
      totalBytesWritten += length
    } else {
      val bytesWritten = limit - position
      value.get(buffer, position, bytesWritten)
      length -= bytesWritten
      position = limit
      totalBytesWritten += bytesWritten
      refreshBuffer()
      while (length > limit) {
        value.get(buffer, 0, limit)
        output.write(buffer, 0, limit)
        length -= limit
        totalBytesWritten += limit
      }
      value.get(buffer, 0, length)
      position = length
      totalBytesWritten += length
    }
  }

  def writeRawBytes(value: Array[Byte], offset0: Int, length0: Int): Unit = {
    var offset = offset0
    var length = length0
    if (limit - position >= length) {
      System.arraycopy(value, offset, buffer, position, length)
      position += length
      totalBytesWritten += length
    } else {
      val bytesWritten = limit - position
      System.arraycopy(value, offset, buffer, position, bytesWritten)
      offset += bytesWritten
      length -= bytesWritten
      position = limit
      totalBytesWritten += bytesWritten
      refreshBuffer()
      if (length <= limit) {
        System.arraycopy(value, offset, buffer, 0, length)
        position = length
      } else {
        output.write(value, offset, length)
      }
      totalBytesWritten += length
    }
  }

}

object Internal {
  val EMPTY_BYTE_ARRAY: Array[Byte] = Array()
}
