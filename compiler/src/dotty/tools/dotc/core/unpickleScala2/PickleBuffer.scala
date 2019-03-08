package dotty.tools
package dotc
package core
package unpickleScala2

import Flags._

/** Variable length byte arrays, with methods for basic pickling and unpickling.
 *
 *  @param data The initial buffer
 *  @param from The first index where defined data are found
 *  @param to   The first index where new data can be written
 */
class PickleBuffer(data: Array[Byte], from: Int, to: Int) {

  var bytes: Array[Byte] = data
  var readIndex: Int = from
  var writeIndex: Int = to

  /** Double bytes array */
  private def dble(): Unit = {
    val bytes1 = new Array[Byte](bytes.length * 2)
    System.arraycopy(bytes, 0, bytes1, 0, writeIndex)
    bytes = bytes1
  }

  def ensureCapacity(capacity: Int): Unit =
    while (bytes.length < writeIndex + capacity) dble()

  // -- Basic output routines --------------------------------------------

  /** Write a byte of data */
  def writeByte(b: Int): Unit = {
    if (writeIndex == bytes.length) dble()
    bytes(writeIndex) = b.toByte
    writeIndex += 1
  }

  /** Write a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def writeNat(x: Int): Unit =
    writeLongNat(x.toLong & 0x00000000FFFFFFFFL)

  /**
   * Like writeNat, but for longs. This is not the same as
   * writeLong, which writes in base 256. Note that the
   * binary representation of LongNat is identical to Nat
   * if the long value is in the range Int.MIN_VALUE to
   * Int.MAX_VALUE.
   */
  def writeLongNat(x: Long): Unit = {
    def writeNatPrefix(x: Long): Unit = {
      val y = x >>> 7
      if (y != 0L) writeNatPrefix(y)
      writeByte(((x & 0x7f) | 0x80).toInt)
    }
    val y = x >>> 7
    if (y != 0L) writeNatPrefix(y)
    writeByte((x & 0x7f).toInt)
  }

  /** Write a natural number <code>x</code> at position <code>pos</code>.
   *  If number is more than one byte, shift rest of array to make space.
   *
   *  @param pos ...
   *  @param x   ...
   */
  def patchNat(pos: Int, x: Int): Unit = {
    def patchNatPrefix(x: Int): Unit = {
      writeByte(0)
      System.arraycopy(bytes, pos, bytes, pos + 1, writeIndex - (pos + 1))
      bytes(pos) = ((x & 0x7f) | 0x80).toByte
      val y = x >>> 7
      if (y != 0) patchNatPrefix(y)
    }
    bytes(pos) = (x & 0x7f).toByte
    val y = x >>> 7
    if (y != 0) patchNatPrefix(y)
  }

  /** Write a long number <code>x</code> in signed big endian format, base 256.
   *
   *  @param x The long number to be written.
   */
  def writeLong(x: Long): Unit = {
    val y = x >> 8
    val z = x & 0xff
    if (-y != (z >> 7)) writeLong(y)
    writeByte(z.toInt)
  }

  // -- Basic input routines --------------------------------------------

  /** Peek at the current byte without moving the read index */
  def peekByte(): Int = bytes(readIndex)

  /** Read a byte */
  def readByte(): Int = {
    val x = bytes(readIndex); readIndex += 1; x
  }

  /** Read a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.*/
  def readNat(): Int = readLongNat().toInt

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte()
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L)
    x
  }

  /** Read a long number in signed big endian format, base 256. */
  def readLong(len: Int): Long = {
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i += 1
    }
    val leading = 64 - (len << 3)
    x << leading >> leading
  }

  /** Returns the buffer as a sequence of (Int, Array[Byte]) representing
   *  (tag, data) of the individual entries.  Saves and restores buffer state.
   */

  def toIndexedSeq: IndexedSeq[(Int, Array[Byte])] = {
    val saved = readIndex
    readIndex = 0
    readNat() ; readNat()     // discarding version
    val result = new Array[(Int, Array[Byte])](readNat())

    result.indices foreach { index =>
      val tag = readNat()
      val len = readNat()
      val bytes = data.slice(readIndex, len + readIndex)
      readIndex += len

      result(index) = tag -> bytes
    }

    readIndex = saved
    result.toIndexedSeq
  }

  /** Perform operation <code>op</code> until the condition
   *  <code>readIndex == end</code> is satisfied.
   *  Concatenate results into a list.
   *
   *  @param end ...
   *  @param op  ...
   *  @return    ...
   */
  def until[T](end: Int, op: () => T): List[T] =
    if (readIndex == end) List() else op() :: until(end, op)

  /** Perform operation <code>op</code> the number of
   *  times specified.  Concatenate the results into a list.
   */
  def times[T](n: Int, op: ()=>T): List[T] =
    if (n == 0) List() else op() :: times(n-1, op)

  /** Pickle = majorVersion_Nat minorVersion_Nat nbEntries_Nat {Entry}
   *  Entry  = type_Nat length_Nat [actual entries]
   *
   *  Assumes that the ..Version_Nat are already consumed.
   *
   *  @return an array mapping entry numbers to locations in
   *  the byte array where the entries start.
   */
  def createIndex: Array[Int] = {
    val index = new Array[Int](readNat()) // nbEntries_Nat
    for (i <- 0 until index.length) {
      index(i) = readIndex
      readByte() // skip type_Nat
      readIndex = readNat() + readIndex // read length_Nat, jump to next entry
    }
    index
  }
}

object PickleBuffer {

  private final val ScalaFlagEnd = 48
  private final val ChunkBits = 8
  private final val ChunkSize = 1 << ChunkBits
  private type FlagMap = Array[Array[Long]]

  private val (scalaTermFlagMap, scalaTypeFlagMap) = {
    import Scala2Flags._

    val corr = Map(
      PROTECTED_PKL -> Protected,
      OVERRIDE_PKL -> Override,
      PRIVATE_PKL -> Private,
      ABSTRACT_PKL -> Abstract,
      DEFERRED_PKL -> Deferred,
      FINAL_PKL -> Final,
      METHOD_PKL -> Method,
      INTERFACE_PKL -> NoInitsInterface,
      MODULE_PKL -> (Module | Lazy, Module),
      IMPLICIT_PKL -> Implicit,
      SEALED_PKL -> Sealed,
      CASE_PKL -> Case,
      MUTABLE -> Mutable,
      PARAM -> Param,
      PACKAGE -> Package,
      MACRO -> Macro,
      BYNAMEPARAM -> (Method, Covariant),
      LABEL -> (Label, Contravariant),
      ABSOVERRIDE -> AbsOverride,
      LOCAL -> Local,
      JAVA -> JavaDefined,
      SYNTHETIC -> Synthetic,
      STABLE -> StableRealizable,
      STATIC -> JavaStatic,
      CASEACCESSOR -> CaseAccessor,
      DEFAULTPARAM -> (DefaultParameterized, Trait),
      BRIDGE -> Bridge,
      ACCESSOR -> Accessor,
      SUPERACCESSOR -> Scala2SuperAccessor,
      PARAMACCESSOR -> ParamAccessor,
      MODULEVAR -> Scala2ModuleVar,
      LAZY -> Lazy,
      MIXEDIN -> (MixedIn, Scala2Existential),
      EXPANDEDNAME -> Scala2ExpandedName,
      SPECIALIZED -> Specialized,
      VBRIDGE -> EmptyFlags,
      VARARGS -> JavaVarargs,
      ENUM -> Enum)

    // generate initial maps from Scala flags to Dotty flags
    val termMap, typeMap = new Array[Long](64)
    for (idx <- 0 until ScalaFlagEnd)
      corr get (1L << idx) match {
        case Some((termFlag: FlagSet, typeFlag: FlagSet)) =>
          termMap(idx) |= termFlag.bits
          typeMap(idx) |= typeFlag.bits
        case Some(commonFlag: FlagSet) =>
          termMap(idx) |= commonFlag.toTermFlags.bits
          typeMap(idx) |= commonFlag.toTypeFlags.bits
        case _ =>
      }

    // Convert map so that it maps chunks of ChunkBits size at once
    // instead of single bits.
    def chunkMap(xs: Array[Long]): FlagMap = {
      val size = (xs.length + ChunkBits - 1) / ChunkBits
      val chunked = Array.ofDim[Long](size, ChunkSize)
      var i = 0
      while (i < size) {
        var j = 0
        while (j < ChunkSize) {
          var k = 0
          while (k < ChunkBits) {
            if ((j & (1 << k)) != 0)
              chunked(i)(j) |= xs(i * ChunkBits + k)
            k += 1
          }
          j += 1
        }
        i += 1
      }
      chunked
    }

    (chunkMap(termMap), chunkMap(typeMap))
  }

  def unpickleScalaFlags(sflags: Long, isType: Boolean): FlagSet = {
    val map: FlagMap = if (isType) scalaTypeFlagMap else scalaTermFlagMap
    val shift = ChunkBits
    val mask = ChunkSize - 1
    assert(6 * ChunkBits == ScalaFlagEnd)
    FlagSet(
      map(0)((sflags >>> (shift * 0)).toInt & mask) |
      map(1)((sflags >>> (shift * 1)).toInt & mask) |
      map(2)((sflags >>> (shift * 2)).toInt & mask) |
      map(3)((sflags >>> (shift * 3)).toInt & mask) |
      map(4)((sflags >>> (shift * 4)).toInt & mask) |
      map(5)((sflags >>> (shift * 5)).toInt & mask)
    )
  }
}
