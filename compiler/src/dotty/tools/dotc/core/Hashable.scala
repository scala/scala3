package dotty.tools.dotc
package core

import Types._
import scala.util.hashing.{ MurmurHash3 => hashing }
import annotation.tailrec

object Hashable {

  /** A null terminated list of BindingTypes. We use `null` here for efficiency */
  class Binders(val tp: BindingType, val next: Binders)

  /** A null terminated list of pairs of BindingTypes. Used for isomorphism tests. */
  class BinderPairs(tp1: BindingType, tp2: BindingType, next: BinderPairs) {
    @tailrec final def matches(t1: Type, t2: Type): Boolean =
      (t1 `eq` tp1) && (t2 `eq` tp2) || next != null && next.matches(t1, t2)
  }

  /** A hash value indicating that the underlying type is not
   *  cached in uniques.
   */
  inline val NotCached = 0

  /** An alternative value returned from `hash` if the
   *  computed hashCode would be `NotCached`.
   */
  private[core] inline val NotCachedAlt = Int.MinValue

  /** A value that indicates that the hash code is unknown
   */
  private[core] inline val HashUnknown = 1234

  /** An alternative value if computeHash would otherwise yield HashUnknown
   */
  private[core] inline val HashUnknownAlt = 4321
}

trait Hashable {
  import Hashable._

  protected def hashSeed: Int = getClass.hashCode

  protected final def finishHash(hashCode: Int, arity: Int): Int =
    avoidSpecialHashes(hashing.finalizeHash(hashCode, arity))

  final def typeHash(bs: Binders, tp: Type): Int =
    if (bs == null || tp.hashIsStable) tp.hash else tp.computeHash(bs)

  def identityHash(bs: Binders): Int = avoidSpecialHashes(System.identityHashCode(this))

  protected def finishHash(bs: Binders, seed: Int, arity: Int, tp: Type): Int = {
    val elemHash = typeHash(bs, tp)
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1)
  }

  protected def finishHash(bs: Binders, seed: Int, arity: Int, tp1: Type, tp2: Type): Int = {
    val elemHash = typeHash(bs, tp1)
    if (elemHash == NotCached) return NotCached
    finishHash(bs, hashing.mix(seed, elemHash), arity + 1, tp2)
  }

  protected def finishHash(bs: Binders, seed: Int, arity: Int, tps: List[Type]): Int = {
    var h = seed
    var xs = tps
    var len = arity
    while (!xs.isEmpty) {
      val elemHash = typeHash(bs, xs.head)
      if (elemHash == NotCached) return NotCached
      h = hashing.mix(h, elemHash)
      xs = xs.tail
      len += 1
    }
    finishHash(h, len)
  }

  protected def finishHash(bs: Binders, seed: Int, arity: Int, tp: Type, tps: List[Type]): Int = {
    val elemHash = typeHash(bs, tp)
    if (elemHash == NotCached) return NotCached
    finishHash(bs, hashing.mix(seed, elemHash), arity + 1, tps)
  }


  protected final def doHash(x: Any): Int =
    finishHash(hashing.mix(hashSeed, x.hashCode), 1)

  protected final def doHash(bs: Binders, tp: Type): Int =
    finishHash(bs, hashSeed, 0, tp)

  protected final def doHash(bs: Binders, x1: Any, tp2: Type): Int =
    finishHash(bs, hashing.mix(hashSeed, x1.hashCode), 1, tp2)

  protected final def doHash(bs: Binders, x1: Int, tp2: Type): Int =
    finishHash(bs, hashing.mix(hashSeed, x1), 1, tp2)

  protected final def doHash(bs: Binders, x1: Int, tp2: Type, tp3: Type): Int =
    finishHash(bs, hashing.mix(hashSeed, x1), 1, tp2, tp3)

  protected final def doHash(bs: Binders, tp1: Type, tp2: Type): Int =
    finishHash(bs, hashSeed, 0, tp1, tp2)

  protected final def doHash(bs: Binders, x1: Any, tp2: Type, tp3: Type): Int =
    finishHash(bs, hashing.mix(hashSeed, x1.hashCode), 1, tp2, tp3)

  protected final def doHash(bs: Binders, tp1: Type, tps2: List[Type]): Int =
    finishHash(bs, hashSeed, 0, tp1, tps2)

  protected final def doHash(bs: Binders, x1: Any, tp2: Type, tps3: List[Type]): Int =
    finishHash(bs, hashing.mix(hashSeed, x1.hashCode), 1, tp2, tps3)

  protected final def doHash(x1: Any, x2: Int): Int =
    finishHash(hashing.mix(hashing.mix(hashSeed, x1.hashCode), x2), 1)

  protected final def doHash(x1: Int, x2: Int): Int =
    finishHash(hashing.mix(hashing.mix(hashSeed, x1), x2), 1)

  protected final def addDelta(elemHash: Int, delta: Int): Int =
    if (elemHash == NotCached) NotCached
    else avoidSpecialHashes(elemHash + delta)

  protected def avoidSpecialHashes(h: Int): Int =
    if (h == NotCached) NotCachedAlt
    else if (h == HashUnknown) HashUnknownAlt
    else h
}
