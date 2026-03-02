// 5
// Finally, an alternative way to fix the original issue,
// by reimplementing `getOrSet` to not even need
// our `get` extension.
import java.util.concurrent.atomic.AtomicReference

opaque type Worm[V] = AtomicReference[AnyRef]
object Worm:
  val notSetSentinel: AnyRef = new AnyRef {}

  extension [V](worm: Worm[V])
    inline def wormAsAtomic: AtomicReference[AnyRef] = worm // deprecate?

    inline def setIfEmpty(v: => V): Boolean =
      val x = worm.get()
      if x eq notSetSentinel then
        val value = v
        worm.set(value.asInstanceOf[AnyRef])
        true
      else false

    inline def get: V =
      val x = worm.get()
      if x eq notSetSentinel then
        throw IllegalStateException("Retrieved value before being set")
      else x.asInstanceOf[V]

    inline def getOrSet(v: => V): V =
      val x = worm.get()
      if x eq notSetSentinel then
        val value = v
        worm.set(value.asInstanceOf[AnyRef])
        value
      else x.asInstanceOf[V]
