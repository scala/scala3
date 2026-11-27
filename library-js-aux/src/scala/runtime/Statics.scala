/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.runtime

/** Not for public consumption.  Usage by the runtime only. */

@annotation.nowarn("msg=method hashCode in class Any is deprecated") // this class is only called under certain assumptions
object Statics {
  def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = Integer.rotateLeft(h, 13)
    (h * 5) + 0xe6546b64
  }

  def mixLast(hash: Int, data: Int): Int = {
    var k = data
    k *= 0xcc9e2d51
    k = Integer.rotateLeft(k, 15)
    k *= 0x1b873593
    hash ^ k
  }

  def finalizeHash(hash: Int, length: Int): Int =
    avalanche(hash ^ length)

  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  def avalanche(h0: Int): Int = {
    var h = h0
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h
  }

  def longHash(lv: Long): Int = {
    val lo = lv.toInt
    val hi = (lv >>> 32).toInt
    if (hi == (lo >> 31)) lo // it is in the Int range
    else lo ^ hi
  }

  def doubleHash(dv: Double): Int = {
    val iv = dv.toInt
    if (iv == dv) {
      iv
    } else {
      // don't test the case dv.toFloat == dv
      val lv = dv.toLong
      if (lv == dv)
        lv.hashCode()
      else
        dv.hashCode()
    }
  }

  def floatHash(fv: Float): Int =
    doubleHash(fv.toDouble)

  def anyHash(x: Any): Int = {
    x match {
      case null      => 0
      case x: Double => doubleHash(x)
      case x: Long   => longHash(x)
      case _         => x.hashCode()
    }
  }

  /** Used as a marker object to return from PartialFunctions */
  def pfMarker: AnyRef = PFMarker

  private object PFMarker extends AnyRef

  def releaseFence(): Unit = ()

  /** Just throws an exception.
   *
   *  Used by the synthetic `productElement` and `productElementName` methods
   *  in case classes. Delegating the exception-throwing to this function
   *  reduces the bytecode size of the case class.
   */
  final def ioobe[T](n: Int): T =
    throw new IndexOutOfBoundsException(String.valueOf(n))
}
