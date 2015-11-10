package dotty.tools.dotc
package core

import Types._
import scala.util.hashing.{ MurmurHash3 => hashing }

object Hashable {

  /** A hash value indicating that the underlying type is not
   *  cached in uniques.
   */
  final val NotCached = 0

  /** An alternative value returned from `hash` if the
   *  computed hashCode would be `NotCached`.
   */
  private[core] final val NotCachedAlt = Int.MinValue

  /** A value that indicates that the hash code is unknown
   */
  private[core] final val HashUnknown = 1234

  /** An alternative value if computeHash would otherwise yield HashUnknown
   */
  private[core] final val HashUnknownAlt = 4321
}

trait Hashable {
  import Hashable._

  protected def hashSeed: Int = getClass.hashCode

  protected final def finishHash(hashCode: Int, arity: Int): Int =
    avoidNotCached(hashing.finalizeHash(hashCode, arity))

  final def identityHash = avoidNotCached(System.identityHashCode(this))

  protected def finishHash(seed: Int, arity: Int, tp: Type): Int = {
    val elemHash = tp.hash
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1)
  }

  protected def finishHash(seed: Int, arity: Int, tp1: Type, tp2: Type): Int = {
    val elemHash = tp1.hash
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1, tp2)
  }

  protected def finishHash(seed: Int, arity: Int, tps: List[Type]): Int = {
    var h = seed
    var xs = tps
    var len = arity
    while (xs.nonEmpty) {
      val elemHash = xs.head.hash
      if (elemHash == NotCached) return NotCached
      h = hashing.mix(h, elemHash)
      xs = xs.tail
      len += 1
    }
    finishHash(h, len)
  }

  protected def finishHash(seed: Int, arity: Int, tp: Type, tps: List[Type]): Int = {
    val elemHash = tp.hash
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1, tps)
  }

  protected final def doHash(x: Any): Int =
    finishHash(hashing.mix(hashSeed, x.hashCode), 1)

  protected final def doHash(tp: Type): Int =
    finishHash(hashSeed, 0, tp)

  protected final def doHash(x1: Any, tp2: Type): Int =
    finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2)

  protected final def doHash(tp1: Type, tp2: Type): Int =
    finishHash(hashSeed, 0, tp1, tp2)

  protected final def doHash(x1: Any, tp2: Type, tp3: Type): Int =
    finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2, tp3)

  protected final def doHash(tp1: Type, tps2: List[Type]): Int =
    finishHash(hashSeed, 0, tp1, tps2)

  protected final def doHash(x1: Any, tp2: Type, tps3: List[Type]): Int =
    finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2, tps3)


  protected final def doHash(x1: Int, x2: Int): Int =
    finishHash(hashing.mix(hashing.mix(hashSeed, x1), x2), 1)

  protected final def addDelta(hc: Int, delta: Int) = avoidNotCached(hc + delta)

  private def avoidNotCached(h: Int) = if (h == NotCached) NotCachedAlt else h
}
