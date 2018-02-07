package dotty.tools.dotc
package core

import Types._
import scala.util.hashing.{ MurmurHash3 => hashing }

abstract class Hashing {
  import Hashing._

  final def finishHash(hashCode: Int, arity: Int): Int =
    avoidSpecialHashes(hashing.finalizeHash(hashCode, arity))

  protected def typeHash(tp: Type) = tp.hash

  def identityHash(tp: Type) = avoidSpecialHashes(System.identityHashCode(tp))

  protected def finishHash(seed: Int, arity: Int, tp: Type): Int = {
    val elemHash = typeHash(tp)
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1)
  }

  protected def finishHash(seed: Int, arity: Int, tp1: Type, tp2: Type): Int = {
    val elemHash = typeHash(tp1)
    if (elemHash == NotCached) return NotCached
    finishHash(hashing.mix(seed, elemHash), arity + 1, tp2)
  }

  protected def finishHash(seed: Int, arity: Int, tps: List[Type]): Int = {
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

  protected def finishHash(seed: Int, arity: Int, tp: Type, tps: List[Type]): Int = {
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

  private def avoidSpecialHashes(h: Int) =
    if (h == NotCached) NotCachedAlt
    else if (h == HashUnknown) HashUnknownAlt
    else h

  val binders: Array[BindingType] = Array()

  private class WithBinders(override val binders: Array[BindingType]) extends Hashing {

    override def typeHash(tp: Type) = tp.computeHash(this)

    override def identityHash(tp: Type) = {
      var idx = 0
      while (idx < binders.length && (binders(idx) `ne` tp))
        idx += 1
      avoidSpecialHashes(
        if (idx < binders.length) idx * 31
        else System.identityHashCode(binders))
    }
  }

  def withBinder(binder: BindingType): Hashing = {
    val newBinders = new Array[BindingType](binders.length + 1)
    Array.copy(binders, 0, newBinders, 0, binders.length)
    newBinders(binders.length) = binder
    new WithBinders(newBinders)
  }
}

object Hashing extends Hashing {

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

