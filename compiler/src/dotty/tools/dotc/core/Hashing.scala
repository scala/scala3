package dotty.tools.dotc
package core

import Types._
import scala.util.hashing.{ MurmurHash3 => hashing }

class Hashing(binders: Array[BindingType]) {
  import Hashing._

  private def avoidSpecialHashes(h: Int) =
    if (h == NotCached) NotCachedAlt
    else if (h == HashUnknown) HashUnknownAlt
    else h

  private def finishHash(hashCode: Int, arity: Int): Int =
    avoidSpecialHashes(hashing.finalizeHash(hashCode, arity))

  private def typeHash(tp: Type) =
    if (binders == null) tp.hash else tp.computeHash(this)

  def identityHash(tp: Type): Int = {
    if (binders != null) {
      var idx = 0
      while (idx < binders.length) {
        if (binders(idx) `eq` tp) return avoidSpecialHashes(idx * 31)
        idx += 1
      }
    }
    avoidSpecialHashes(System.identityHashCode(tp))
  }

  private def finishHash(seed: Int, arity: Int, tp: Type): Int = {
    val elemHash = typeHash(tp)
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1)
  }

  private def finishHash(seed: Int, arity: Int, tp1: Type, tp2: Type): Int = {
    val elemHash = typeHash(tp1)
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1, tp2)
  }

  private def finishHash(seed: Int, arity: Int, tps: List[Type]): Int = {
    var h = seed
    var xs = tps
    var len = arity
    while (xs.nonEmpty) {
      val elemHash = typeHash(xs.head)
      if (elemHash == NotCached) return NotCached
      h = hashing.mix(h, elemHash)
      xs = xs.tail
      len += 1
    }
    finishHash(h, len)
  }

  private def finishHash(seed: Int, arity: Int, tp: Type, tps: List[Type]): Int = {
    val elemHash = typeHash(tp)
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1, tps)
  }

  final def doHash(clazz: Class[_], x: Any): Int =
    finishHash(hashing.mix(clazz.hashCode, x.hashCode), 1)

  final def doHash(clazz: Class[_], tp: Type): Int =
    finishHash(clazz.hashCode, 0, tp)

  final def doHash(clazz: Class[_], x1: Any, tp2: Type): Int =
    finishHash(hashing.mix(clazz.hashCode, x1.hashCode), 1, tp2)

  final def doHash(clazz: Class[_], tp1: Type, tp2: Type): Int =
    finishHash(clazz.hashCode, 0, tp1, tp2)

  final def doHash(clazz: Class[_], x1: Any, tp2: Type, tp3: Type): Int =
    finishHash(hashing.mix(clazz.hashCode, x1.hashCode), 1, tp2, tp3)

  final def doHash(clazz: Class[_], tp1: Type, tps2: List[Type]): Int =
    finishHash(clazz.hashCode, 0, tp1, tps2)

  final def doHash(clazz: Class[_], x1: Any, tp2: Type, tps3: List[Type]): Int =
    finishHash(hashing.mix(clazz.hashCode, x1.hashCode), 1, tp2, tps3)

  final def doHash(clazz: Class[_], x1: Int, x2: Int): Int =
    finishHash(hashing.mix(hashing.mix(clazz.hashCode, x1), x2), 1)

  final def addDelta(elemHash: Int, delta: Int) =
    if (elemHash == NotCached) NotCached
    else avoidSpecialHashes(elemHash + delta)

  final def withBinder(binder: BindingType): Hashing = {
    new Hashing(
      if (binders == null) {
        val bs = new Array[BindingType](1)
        bs(0) = binder
        bs
      }
      else {
        val bs = new Array[BindingType](binders.length + 1)
        Array.copy(binders, 0, bs, 0, binders.length)
        bs(binders.length) = binder
        bs
      })
  }
}

object Hashing extends Hashing(null) {

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

