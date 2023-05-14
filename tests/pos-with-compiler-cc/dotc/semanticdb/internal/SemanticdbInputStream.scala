package dotty.tools.dotc.semanticdb.internal

import scala.language.unsafeNulls

import java.io.IOException
import java.io.InputStream
import java.util.Arrays
import java.nio.charset.StandardCharsets

import SemanticdbInputStream._

import scala.collection.mutable

object SemanticdbInputStream {

  def newInstance(input: InputStream): SemanticdbInputStream = new SemanticdbInputStream(input)

  def newInstance(buf: Array[Byte]): SemanticdbInputStream = newInstance(buf, 0, buf.length)

  def newInstance(buf: Array[Byte], off: Int, len: Int): SemanticdbInputStream = {
    val result = new SemanticdbInputStream(buf, off, len)
    result.pushLimit(len)
    result
  }

  @throws[InvalidProtocolBufferException]
  def readRawVarint32(input: InputStream): Int = {
    val firstByte = input.read()
    if (firstByte == -1) {
      throw InvalidProtocolBufferException.truncatedMessage()
    }
    readRawVarint32(firstByte, input)
  }

  def readRawVarint32(firstByte: Int, input: InputStream): Int = {
    if ((firstByte & 0x80) == 0) {
      return firstByte
    }
    var result = firstByte & 0x7f
    var offset = 7
    while (offset < 32) {
      val b = input.read()
      if (b == -1) {
        throw InvalidProtocolBufferException.truncatedMessage()
      }
      result |= (b & 0x7f) << offset
      if ((b & 0x80) == 0) {
        return result
      }
      offset += 7
    }
    while (offset < 64) {
      val b = input.read()
      if (b == -1) {
        throw InvalidProtocolBufferException.truncatedMessage()
      }
      if ((b & 0x80) == 0) {
        return result
      }
      offset += 7
    }
    throw InvalidProtocolBufferException.malformedVarint()
  }

  def decodeZigZag32(n: Int): Int = (n >>> 1) ^ -(n & 1)

  def decodeZigZag64(n: Long): Long = (n >>> 1) ^ -(n & 1)

  private val DEFAULT_RECURSION_LIMIT = 100

  private val DEFAULT_SIZE_LIMIT = 64 << 20

  private val BUFFER_SIZE = 4096
}

class SemanticdbInputStream private (buffer: Array[Byte], input: InputStream) {
  /**
    * The total number of bytes read before the current buffer.  The total
    * bytes read up to the current position can be computed as
    * {@code totalBytesRetired + bufferPos}.  This value may be negative if
    * reading started in the middle of the current buffer (e.g. if the
    * constructor that takes a byte array and an offset was used).
    */
  private var totalBytesRetired: Int = 0

  // Current position in the buffer.
  private var bufferPos: Int = 0

  // How many bytes in the buffer contain actual content (bufferSize <= buffer.length)
  private var bufferSize: Int = 0

  private var currentLimit: Int = Int.MaxValue

  private var sizeLimit = SemanticdbInputStream.DEFAULT_SIZE_LIMIT

  private var bufferSizeAfterLimit = 0

  private var lastTag = 0

  def this(buffer: Array[Byte], offset: Int, len: Int) = {
    this(buffer, null)
    bufferPos = offset
    bufferSize = offset + len
    totalBytesRetired = -offset
  }

  def this(is: InputStream) = {
    this(new Array[Byte](SemanticdbInputStream.BUFFER_SIZE), is)
    totalBytesRetired = 0
  }

  /**
    * Ensures that at least {@code n} bytes are available in the buffer, reading
    * more bytes from the input if necessary to make it so.  Caller must ensure
    * that the requested space is less than BUFFER_SIZE.
    */
  private def ensureAvailable(n: Int): Unit = {
    if (bufferSize - bufferPos < n) {
      refillBuffer(n)
    }
  }

  /**
    * Reads more bytes from the input, making at least {@code n} bytes available
    * in the buffer.  Caller must ensure that the requested space is not yet
    * available, and that the requested space is less than BUFFER_SIZE.
    */
  private def refillBuffer(n: Int): Unit = {
    if (!tryRefillBuffer(n)) {
      throw InvalidProtocolBufferException.truncatedMessage()
    }
  }
  /**
    * Tries to read more bytes from the input, making at least {@code n} bytes
    * available in the buffer.  Caller must ensure that the requested space is
    * not yet available, and that the requested space is less than BUFFER_SIZE.
    *
    * @return { @code true} if the bytes could be made available; { @code false}
    *                                                                     if the end of the stream or the current limit was reached.
    */
  private def tryRefillBuffer(n: Int): Boolean = {
    if (bufferPos + n <= bufferSize) {
      throw new IllegalStateException(
        s"refillBuffer() called when $n bytes were already available in buffer")
    }
    if totalBytesRetired + bufferPos + n <= currentLimit && input != null then
      val pos: Int = bufferPos
      if (pos > 0) {
        if (bufferSize > pos) {
          System.arraycopy(buffer, pos, buffer, 0, bufferSize - pos)
        }
        totalBytesRetired += pos
        bufferSize -= pos
        bufferPos = 0
      }
      val bytesRead: Int = input.read(buffer, bufferSize, buffer.length - bufferSize)
      if (bytesRead == 0 || bytesRead < -1 || bytesRead > buffer.length) {
        throw new IllegalStateException("InputStream#read(byte[]) returned invalid result: " + bytesRead + "\nThe InputStream implementation is buggy.")
      }
      if (bytesRead > 0) {
        bufferSize += bytesRead
        if (totalBytesRetired + n - sizeLimit > 0) {
          throw InvalidProtocolBufferException.sizeLimitExceeded()
        }
        recomputeBufferSizeAfterLimit()
        return ((bufferSize >= n) || tryRefillBuffer(n))
      }
    false
  }

  private def recomputeBufferSizeAfterLimit(): Unit = {
    bufferSize += bufferSizeAfterLimit
    val bufferEnd: Int = totalBytesRetired + bufferSize
    if (bufferEnd > currentLimit) {
      bufferSizeAfterLimit = bufferEnd - currentLimit
      bufferSize -= bufferSizeAfterLimit
    }
    else {
      bufferSizeAfterLimit = 0
    }
  }

  /**
    * Returns true if the stream has reached the end of the input.  This is the
    * case if either the end of the underlying input source has been reached or
    * if the stream has reached a limit created using {@link #pushLimit(int)}.
    */
  def isAtEnd: Boolean = {
    bufferPos == bufferSize && !tryRefillBuffer(1)
  }

  def getTotalBytesRead() = {
    totalBytesRetired + bufferPos
  }

  /**
    * Sets {@code currentLimit} to (current position) + {@code byteLimit}.  This
    * is called when descending into a length-delimited embedded message.
    *
    * <p>Note that {@code pushLimit()} does NOT affect how many bytes the
    * {@code SemanticdbInputStream} reads from an underlying {@code InputStream} when
    * refreshing its buffer.  If you need to prevent reading past a certain
    * point in the underlying {@code InputStream} (e.g. because you expect it to
    * contain more data after the end of the message which you need to handle
    * differently) then you must place a wrapper around your {@code InputStream}
    * which limits the amount of data that can be read from it.
    *
    * @return the old limit.
    */
  def pushLimit(byteLimit0: Int): Int = {
    if (byteLimit0 < 0) {
      throw InvalidProtocolBufferException.negativeSize()
    }
    val byteLimit = byteLimit0 + totalBytesRetired + bufferPos
    val oldLimit: Int = currentLimit
    if (byteLimit > oldLimit) {
      throw InvalidProtocolBufferException.truncatedMessage()
    }
    currentLimit = byteLimit
    recomputeBufferSizeAfterLimit()
    oldLimit
  }

  /**
    * Discards the current limit, returning to the previous limit.
    *
    * @param oldLimit The old limit, as returned by { @code pushLimit}.
    */
  def popLimit(oldLimit: Int): Unit = {
    currentLimit = oldLimit
    recomputeBufferSizeAfterLimit()
  }

  /**
    * Reads and discards a single field, given its tag value.
    *
    * @return { @code false} if the tag is an endgroup tag, in which case
    *                 nothing is skipped.  Otherwise, returns { @code true}.
    */
  @throws(classOf[IOException])
  def skipField(tag: Int): Boolean = {
    WireFormat.getTagWireType(tag) match {
      case WireFormat.WIRETYPE_VARINT =>
        skipRawVarint()
        true
      case WireFormat.WIRETYPE_FIXED64 =>
        skipRawBytes(8)
        true
      case WireFormat.WIRETYPE_LENGTH_DELIMITED =>
        skipRawBytes(readRawVarint32())
        true
      case WireFormat.WIRETYPE_START_GROUP =>
        skipMessage()
        checkLastTagWas(WireFormat.makeTag(WireFormat.getTagFieldNumber(tag), WireFormat.WIRETYPE_END_GROUP))
        true
      case WireFormat.WIRETYPE_END_GROUP =>
        false
      case WireFormat.WIRETYPE_FIXED32 =>
        skipRawBytes(4)
        true
      case _ =>
        throw InvalidProtocolBufferException.invalidWireType()
    }
  }

  /**
    * Reads and discards an entire message.  This will read either until EOF
    * or until an endgroup tag, whichever comes first.
    */
  def skipMessage(): Unit = {
    while (true) {
      val tag: Int = readTag()
      if (tag == 0 || !skipField(tag)) {
        return
      }
    }
  }

  /**
    * Reads and discards {@code size} bytes.
    */
  def skipRawBytes(size: Int): Unit = {
    if (size <= (bufferSize - bufferPos) && size >= 0) {
      bufferPos += size
    }
    else {
      skipRawBytesSlowPath(size)
    }
  }

  /**
    * Read a raw Varint from the stream.  If larger than 32 bits, discard the
    * upper bits.
    */
  @throws[InvalidProtocolBufferException]
  def readRawVarint32(): Int = {
    {
      var pos: Int = bufferPos
      if (bufferSize == pos) {
        return readRawVarint64SlowPath().toInt
      }
      val buffer: Array[Byte] = this.buffer
      var x: Int = 0
      if ((({
        x = buffer(({
          pos += 1; pos - 1
        })); x
      })) >= 0) {
        bufferPos = pos
        return x
      }
      else if (bufferSize - pos < 9) {
        return readRawVarint64SlowPath().toInt
      }
      else if ((({
        x ^= (buffer(({
          pos += 1; pos - 1
        })) << 7); x
      })) < 0) {
        x ^= (~0 << 7)
      }
      else if ((({
        x ^= (buffer(({
          pos += 1; pos - 1
        })) << 14); x
      })) >= 0) {
        x ^= (~0 << 7) ^ (~0 << 14)
      }
      else if ((({
        x ^= (buffer(({
          pos += 1; pos - 1
        })) << 21); x
      })) < 0) {
        x ^= (~0 << 7) ^ (~0 << 14) ^ (~0 << 21)
      }
      else {
        val y: Int = buffer(({
          pos += 1; pos - 1
        }))
        x ^= y << 28
        x ^= (~0 << 7) ^ (~0 << 14) ^ (~0 << 21) ^ (~0 << 28)
        if (y < 0 && buffer(({
          pos += 1; pos - 1
        })) < 0 && buffer(({
          pos += 1; pos - 1
        })) < 0 && buffer(({
          pos += 1; pos - 1
        })) < 0 && buffer(({
          pos += 1; pos - 1
        })) < 0 && buffer(({
          pos += 1; pos - 1
        })) < 0) {
          return readRawVarint64SlowPath().toInt
        }
      }
      bufferPos = pos
      return x
    } //todo: labels is not supported
  }

  private def skipRawVarint(): Unit = {
    if (bufferSize - bufferPos >= 10) {
      val buffer: Array[Byte] = this.buffer
      var pos: Int = bufferPos
      var i: Int = 0
      while (i < 10) {
        {
          if (buffer(({
            pos += 1; pos - 1
          })) >= 0) {
            bufferPos = pos
            return
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
    skipRawVarintSlowPath
  }

  @throws(classOf[IOException])
  private def skipRawVarintSlowPath: Unit = {
    var i: Int = 0
    while (i < 10) {
      if (readRawByte() >= 0) {
        return
      }
      i += 1; i - 1
    }
    throw InvalidProtocolBufferException.malformedVarint()
  }

  /**
    * Exactly like skipRawBytes, but caller must have already checked the fast
    * path: (size <= (bufferSize - pos) && size >= 0)
    */
  private def skipRawBytesSlowPath(size: Int): Unit = {
    if (size < 0) {
      throw InvalidProtocolBufferException.negativeSize()
    }
    if (totalBytesRetired + bufferPos + size > currentLimit) {
      skipRawBytes(currentLimit - totalBytesRetired - bufferPos)
      throw InvalidProtocolBufferException.truncatedMessage()
    }
    var pos: Int = bufferSize - bufferPos
    bufferPos = bufferSize
    refillBuffer(1)
    while (size - pos > bufferSize) {
      pos += bufferSize
      bufferPos = bufferSize
      refillBuffer(1)
    }
    bufferPos = size - pos
  }

  /**
    * Attempt to read a field tag, returning zero if we have reached EOF.
    * Protocol message parsers use this to read tags, since a protocol message
    * may legally end wherever a tag occurs, and zero is not a valid tag number.
    */
  @throws[InvalidProtocolBufferException]
  def readTag(): Int = {
    if (isAtEnd) {
      lastTag = 0
      return 0
    }
    lastTag = readRawVarint32()
    if (WireFormat.getTagFieldNumber(lastTag) == 0) {
      throw InvalidProtocolBufferException.invalidTag()
    }
    lastTag
  }

  def readString(): String = {
    val size: Int = readRawVarint32()
    if (size <= (bufferSize - bufferPos) && size > 0) {
      val result: String = new String(buffer, bufferPos, size, StandardCharsets.UTF_8)
      bufferPos += size
      return result
    }
    else if (size == 0) {
      return ""
    }
    else {
      return new String(readRawBytesSlowPath(size), StandardCharsets.UTF_8)
    }
  }

  def readStringRequireUtf8(): String = {
    val size: Int = readRawVarint32()
    var bytes: Array[Byte] = Array()
    var pos = bufferPos;
    if (size <= (bufferSize - pos) && size > 0) {
      // Fast path:  We already have the bytes in a contiguous buffer, so
      //   just copy directly from it.
      bytes = buffer;
      bufferPos = pos + size;
    } else if (size == 0) {
      return "";
    } else {
      // Slow path:  Build a byte array first then copy it.
      bytes = readRawBytesSlowPath(size);
      pos = 0;
    }
    // TODO(martinrb): We could save a pass by validating while decoding.
    // if (!Utf8.isValidUtf8(bytes, pos, pos + size)) {
    //   throw InvalidProtocolBufferException.invalidUtf8();
    // }
    return new String(bytes, pos, size, "UTF-8");
  }

  def checkLastTagWas(value: Int): Unit = {
    if (lastTag != value) {
      throw InvalidProtocolBufferException.invalidEndTag();
    }
  }

  def getBytesUntilLimit: Int = {
    if (currentLimit == Integer.MAX_VALUE) {
      return -1
    }

    val currentAbsolutePosition: Int = totalBytesRetired + bufferPos
    return currentLimit - currentAbsolutePosition
  }

  /** Read a {@code double} field value from the stream. */
  def readDouble(): Double = {
    return java.lang.Double.longBitsToDouble(readRawLittleEndian64())
  }

  /** Read a {@code float} field value from the stream. */
  def readFloat(): Float = {
    java.lang.Float.intBitsToFloat(readRawLittleEndian32())
  }

  /** Read a {@code uint64} field value from the stream. */
  def readUInt64(): Long = {
    readRawVarint64()
  }

  /** Read an {@code int64} field value from the stream. */
  def readInt64(): Long = {
    readRawVarint64()
  }

  /** Read an {@code int32} field value from the stream. */
  def readInt32(): Int = {
    readRawVarint32()
  }

  /** Read a {@code fixed64} field value from the stream. */
  def readFixed64(): Long = {
    readRawLittleEndian64()
  }

  /** Read a {@code fixed32} field value from the stream. */
  def readFixed32(): Int = {
    readRawLittleEndian32()
  }

  /** Read a {@code uint32} field value from the stream. */
  def readUInt32(): Int = {
    readRawVarint32()
  }

  /**
    * Read an enum field value from the stream.  Caller is responsible
    * for converting the numeric value to an actual enum.
    */
  def readEnum(): Int = {
    readRawVarint32()
  }

  /** Read an {@code sfixed32} field value from the stream. */
  def readSFixed32(): Int = {
    readRawLittleEndian32()
  }

  /** Read an {@code sfixed64} field value from the stream. */
  def readSFixed64(): Long = {
    readRawLittleEndian64()
  }

  /** Read an {@code sint32} field value from the stream. */
  def readSInt32(): Int = {
    decodeZigZag32(readRawVarint32())
  }

  /** Read an {@code sint64} field value from the stream. */
  def readSInt64(): Long = {
    decodeZigZag64(readRawVarint64())
  }

  /** Read a {@code bool} field value from the stream. */
  def readBool(): Boolean = {
    readRawVarint64() != 0
  }

  /** Read a raw Varint from the stream. */
  @throws[InvalidProtocolBufferException]
  def readRawVarint64(): Long = {
    var pos: Int = bufferPos
    if (bufferSize == pos) {
      return readRawVarint64SlowPath()
    }
    val buffer: Array[Byte] = this.buffer
    var x: Long = 0L
    var y: Int = 0
    if ((({
      y = buffer(({
        pos += 1; pos - 1
      })); y
    })) >= 0) {
      bufferPos = pos
      return y
    }
    else if (bufferSize - pos < 9) {
      return readRawVarint64SlowPath()
    }
    else if ((({
      y ^= (buffer(({
        pos += 1; pos - 1
      })) << 7); y
    })) < 0) {
      x = y ^ (~0 << 7)
    }
    else if ((({
      y ^= (buffer(({
        pos += 1; pos - 1
      })) << 14); y
    })) >= 0) {
      x = y ^ ((~0 << 7) ^ (~0 << 14))
    }
    else if ((({
      y ^= (buffer(({
        pos += 1; pos - 1
      })) << 21); y
    })) < 0) {
      x = y ^ ((~0 << 7) ^ (~0 << 14) ^ (~0 << 21))
    }
    else if ((({
      x = (y.toLong) ^ (buffer(({
        pos += 1; pos - 1
      })).toLong << 28); x
    })) >= 0L) {
      x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28)
    }
    else if ((({
      x ^= (buffer(({
        pos += 1; pos - 1
      })).toLong << 35); x
    })) < 0L) {
      x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28) ^ (~0L << 35)
    }
    else if ((({
      x ^= (buffer(({
        pos += 1; pos - 1
      })).toLong << 42); x
    })) >= 0L) {
      x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28) ^ (~0L << 35) ^ (~0L << 42)
    }
    else if ((({
      x ^= (buffer(({
        pos += 1; pos - 1
      })).toLong << 49); x
    })) < 0L) {
      x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28) ^ (~0L << 35) ^ (~0L << 42) ^ (~0L << 49)
    }
    else {
      x ^= (buffer(({
        pos += 1; pos - 1
      })).toLong << 56)
      x ^= (~0L << 7) ^ (~0L << 14) ^ (~0L << 21) ^ (~0L << 28) ^ (~0L << 35) ^ (~0L << 42) ^ (~0L << 49) ^ (~0L << 56)
      if (x < 0L) {
        if (buffer(({
          pos += 1; pos - 1
        })) < 0L) {
          return readRawVarint64SlowPath()
        }
      }
    }
    bufferPos = pos
    x
  }

  /** Variant of readRawVarint64 for when uncomfortably close to the limit. */
  @throws[InvalidProtocolBufferException]
  private[semanticdb] def readRawVarint64SlowPath(): Long = {
    var result: Long = 0
    var shift: Int = 0
    while (shift < 64) {
      val b: Byte = readRawByte()
      result |= (b & 0x7F).toLong << shift
      if ((b & 0x80) == 0) {
        return result
      }
      shift += 7
    }
    throw InvalidProtocolBufferException.malformedVarint()
  }

  /** Read a 32-bit little-endian integer from the stream. */
  def readRawLittleEndian32(): Int = {
    var pos: Int = bufferPos
    if (bufferSize - pos < 4) {
      refillBuffer(4)
      pos = bufferPos
    }
    val buffer: Array[Byte] = this.buffer
    bufferPos = pos + 4
    (((buffer(pos) & 0xff)) |
      ((buffer(pos + 1) & 0xff) << 8) |
      ((buffer(pos + 2) & 0xff) << 16) |
      ((buffer(pos + 3) & 0xff) << 24))
  }

  /** Read a 64-bit little-endian integer from the stream. */
  def readRawLittleEndian64(): Long = {
    var pos: Int = bufferPos
    if (bufferSize - pos < 8) {
      refillBuffer(8)
      pos = bufferPos
    }
    val buffer: Array[Byte] = this.buffer
    bufferPos = pos + 8
    (((buffer(pos).toLong & 0xffL)) |
      ((buffer(pos + 1).toLong & 0xffL) << 8) |
      ((buffer(pos + 2).toLong & 0xffL) << 16) |
      ((buffer(pos + 3).toLong & 0xffL) << 24) |
      ((buffer(pos + 4).toLong & 0xffL) << 32) |
      ((buffer(pos + 5).toLong & 0xffL) << 40) |
      ((buffer(pos + 6).toLong & 0xffL) << 48) |
      ((buffer(pos + 7).toLong & 0xffL) << 56))
  }

  /**
    * Read one byte from the input.
    */
  @throws[InvalidProtocolBufferException]
  def readRawByte(): Byte = {
    if (bufferPos == bufferSize) {
      refillBuffer(1)
    }
    buffer({
      bufferPos += 1; bufferPos - 1
    })
  }

  /**
    * Read a fixed size of bytes from the input.
    */
  @throws[InvalidProtocolBufferException]
  def readRawBytes(size: Int): Array[Byte] = {
    val pos: Int = bufferPos
    if (size <= (bufferSize - pos) && size > 0) {
      bufferPos = pos + size
      Arrays.copyOfRange(buffer, pos, pos + size)
    }
    else {
      readRawBytesSlowPath(size)
    }
  }

  /**
    * Exactly like readRawBytes, but caller must have already checked the fast
    * path: (size <= (bufferSize - pos) && size > 0)
    */
  private def readRawBytesSlowPath(size: Int): Array[Byte] = {
    if (size <= 0) {
      if (size == 0) {
        return Internal.EMPTY_BYTE_ARRAY
      }
      else {
        throw InvalidProtocolBufferException.negativeSize()
      }
    }
    if (totalBytesRetired + bufferPos + size > currentLimit) {
      skipRawBytes(currentLimit - totalBytesRetired - bufferPos)
      throw InvalidProtocolBufferException.truncatedMessage()
    }
    if (size < BUFFER_SIZE) {
      val bytes: Array[Byte] = new Array[Byte](size)
      val pos: Int = bufferSize - bufferPos
      System.arraycopy(buffer, bufferPos, bytes, 0, pos)
      bufferPos = bufferSize
      ensureAvailable(size - pos)
      System.arraycopy(buffer, 0, bytes, pos, size - pos)
      bufferPos = size - pos
      bytes
    }
    else {
      val originalBufferPos: Int = bufferPos
      val originalBufferSize: Int = bufferSize
      totalBytesRetired += bufferSize
      bufferPos = 0
      bufferSize = 0
      var sizeLeft: Int = size - (originalBufferSize - originalBufferPos)
      val chunks: mutable.ArrayBuffer[Array[Byte]] = new mutable.ArrayBuffer[Array[Byte]]
      while (sizeLeft > 0) {
        val chunk: Array[Byte] = new Array[Byte](Math.min(sizeLeft, BUFFER_SIZE))
        var pos: Int = 0
        while (pos < chunk.length) {
          val n: Int = if ((input == null)) -1 else input.read(chunk, pos, chunk.length - pos)
          if (n == -1) {
            throw InvalidProtocolBufferException.truncatedMessage()
          }
          totalBytesRetired += n
          pos += n
        }
        sizeLeft -= chunk.length
        chunks+=(chunk)
      }
      val bytes: Array[Byte] = new Array[Byte](size)
      var pos: Int = originalBufferSize - originalBufferPos
      System.arraycopy(buffer, originalBufferPos, bytes, 0, pos)
      for (chunk <- chunks) {
        System.arraycopy(chunk, 0, bytes, pos, chunk.length)
        pos += chunk.length
      }
      bytes
    }
  }

  def enableAliasing(aliasing: Boolean): Unit = {}

  def setSizeLimit(limit: Int): Int = {
    if (limit < 0) {
      throw new IllegalArgumentException(
        "Size limit cannot be negative: " + limit)
    }
    val oldLimit: Int = sizeLimit
    sizeLimit = limit
    oldLimit
  }

  def resetSizeCounter(): Unit = {
    totalBytesRetired = -bufferPos;
  }
}
