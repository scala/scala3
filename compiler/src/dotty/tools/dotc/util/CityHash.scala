package dotty.tools.dotc.util

/* Ported from https://github.com/google/cityhash/blob/master/src/city.cc */
private[util] class CityHash {

  // Some primes between 2^63 and 2^64 for various uses.
  final val k0 = 0xc3a5c85c97cb3127L
  final val k1 = 0xb492b66fbe98f273L
  final val k2 = 0x9ae16a3b2f90404fL

  protected final def cityHash64(data: Array[Byte]): Long = {
    implicit val implicitData: Array[Byte] = data
    var s = 0
    var len = data.length
    if (len <= 32) {
      if (len <= 16) hashLen0to16(len)
      else hashLen17to32(len)
    } else if (len <= 64) {
      hashLen33to64(len)
    } else {
      // For strings over 64 bytes we hash the end first, and then as we
      // loop we keep 56 bytes of state: v, w, x, y, and z.
      var x: Long = fetch64(s + len - 40)
      var y: Long = fetch64(s + len - 16) + fetch64(s + len - 56)
      var z: Long = hashLen16(fetch64(s + len - 48) + len, fetch64(s + len - 24))
      var v: (Long, Long) = weakHashLen32WithSeeds(s + len - 64, len, z)
      var w: (Long, Long) = weakHashLen32WithSeeds(s + len - 32, y + k1, x)
      x = x * k1 + fetch64(s)

      // Decrease len to the nearest multiple of 64, and operate on 64-byte chunks.
      len = (len - 1) & ~63
      do {
        x = rotate(x + y + v._1 + fetch64(s + 8), 37) * k1
        y = rotate(y + v._2 + fetch64(s + 48), 42) * k1
        x ^= w._2
        y += v._1 + fetch64(s + 40)
        z = rotate(z + w._1, 33) * k1
        v = weakHashLen32WithSeeds(s, v._2 * k1, x + w._1)
        w = weakHashLen32WithSeeds(s + 32, z + w._2, y + fetch64(s + 16))
        val tmp = z
        z = x
        x = tmp
        s += 64
        len -= 64
      } while (len != 0)

      hashLen16(hashLen16(v._1, w._1) + shiftMix(y) * k1 + z,hashLen16(v._2, w._2) + x)
    }
  }

  private final def hashLen0to16(len: Int)(implicit data: Array[Byte]): Long = {
    if (len >= 8) {
      val mul: Long = k2 + len * 2
      val a: Long = fetch64(0) + k2
      val b: Long = fetch64(len - 8)
      val c: Long = rotate(b, 37) * mul + a
      val d: Long = (rotate(a, 25) + b) * mul
      hashLen16(c, d, mul)
    } else if (len >= 4) {
      val mul = k2 + len * 2
      val a = fetch32(0)
      hashLen16(len + (a << 3), fetch32(len - 4), mul)
    } else if (len > 0) {
      val a: Byte = data(0)
      val b: Byte = data(len >> 1)
      val c: Byte = data(len - 1)
      val y: Int = a.toInt + (b.toInt << 8)
      val z: Int = len + (c.toInt << 2)
      shiftMix(y * k2 ^ z * k0) * k2
    } else {
      k2
    }
  }

  // This probably works well for 16-byte strings as well, but it may be overkill
  // in that case.
  private final def hashLen17to32(len: Int)(implicit data: Array[Byte]): Long = {
    val mul: Long = k2 + len * 2
    val a: Long = fetch64(0) * k1
    val b: Long = fetch64(8)
    val c: Long = fetch64(len - 8) * mul
    val d: Long = fetch64(len - 16) * k2
    hashLen16(rotate(a + b, 43) + rotate(c, 30) + d, a + rotate(b + k2, 18) + c, mul)
  }

  /** Return an 8-byte hash for 33 to 64 bytes. */
  private final def hashLen33to64(len: Int)(implicit data: Array[Byte]): Long = {
    val mul: Long = k2 + len * 2
    val a = fetch64(0) * k2
    val b = fetch64(8)
    val c = fetch64(len - 24)
    val d = fetch64(len - 32)
    val e = fetch64(16) * k2
    val f = fetch64(24) * 9
    val g = fetch64(len - 8)
    val h = fetch64(len - 16) * mul
    val u = rotate(a + g, 43) + (rotate(b, 30) + c) * 9
    val v = ((a + g) ^ d) + f + 1
    val w = bswap64((u + v) * mul) + h
    val x = rotate(e + f, 42) + c
    val y = (bswap64((v + w) * mul) + g) * mul
    val z = e + f + c
    val a2 = bswap64((x + z) * mul + y) + b
    shiftMix((z + a2) * mul + d + h) * mul + x
  }

  private final def hashLen16(hi: Long, lo: Long): Long = {
    val kMul = 0x9ddfea08eb382d69L
    var a = (lo ^ hi) * kMul
    a ^= (a >> 47)
    var b = (hi ^ a) * kMul
    b ^= (b >> 47)
    b * kMul
  }

  private final def hashLen16(u: Long, v: Long, mul: Long): Long = {
    // Murmur-inspired hashing.
    var a = (u ^ v) * mul
    a ^= (a >> 47)
    a = (v ^ a) * mul
    a ^= (a >> 47)
    a * mul
  }

  /** Return a 16-byte hash for 48 bytes.  Quick and dirty.
   *  Callers do best to use "random-looking" values for a and b.
   */
  private final def weakHashLen32WithSeeds(w: Long, x: Long, y: Long, z: Long, a0: Long, b0: Long): (Long, Long) = {
    var a = a0
    var b = b0
    a += w
    b = rotate(b + a + z, 21)
    val c: Long = a
    a += x
    a += y
    b += rotate(a, 44)
    (a + z, b + c)
  }

  /** Return a 16-byte hash for s[0] ... s[31], a, and b.  Quick and dirty. */
  private final def weakHashLen32WithSeeds(s: Int, a: Long, b: Long)(implicit data: Array[Byte]): (Long, Long) =
    weakHashLen32WithSeeds(fetch64(s), fetch64(s + 8), fetch64(s + 16), fetch64(s + 24),  a, b)

  private final def fetch64(idx: Int)(implicit data: Array[Byte]): Long = {
    var x: Long = data(idx)
    x = data(idx + 1) | (x << 8)
    x = data(idx + 2) | (x << 8)
    x = data(idx + 3) | (x << 8)
    x = data(idx + 4) | (x << 8)
    x = data(idx + 5) | (x << 8)
    x = data(idx + 6) | (x << 8)
    data(idx + 7) | (x << 8)
  }

  private final def fetch32(idx: Int)(implicit data: Array[Byte]): Long = {
    var x: Int = data(idx)
    x = data(idx + 1) | (x << 8)
    x = data(idx + 2) | (x << 8)
    data(idx + 3) | (x << 8)
  }

  private final def bswap64(x: Long): Long = {
    ((x & 0xff00000000000000L) >> 56) |
    ((x & 0x00ff000000000000L) >> 40) |
    ((x & 0x0000ff0000000000L) >> 24) |
    ((x & 0x000000ff00000000L) >>  8) |
    ((x & 0x00000000ff000000L) <<  8) |
    ((x & 0x0000000000ff0000L) << 24) |
    ((x & 0x000000000000ff00L) << 40) |
    ((x & 0x00000000000000ffL) << 56)
  }

  // Bitwise right rotate.  Normally this will compile to a single
  // instruction, especially if the shift is a manifest constant.
  private final def rotate(v: Long, shift: Int): Long = {
    // Avoid shifting by 64: doing so yields an undefined result.
    if (shift == 0) v else (v >> shift) | (v << (64 - shift))
  }

  private final def shiftMix(v: Long): Long = v ^ (v >> 47)

}

object CityHash extends CityHash {
  def bytesHash(data: Array[Byte]): Long = cityHash64(data)
}
