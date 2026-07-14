// 1
// A re-minimisated reproduction of the original issue in kse3
// The one in the issue removes the usage of the package
// in the second extension bundle, which is crucial to
// why my change broke this code
package kse.flow

import java.util.concurrent.atomic.AtomicReference

opaque type Worm[V] = AtomicReference[AnyRef]
object Worm:
  val notSetSentinel: AnyRef = new AnyRef {}

  extension [V](worm: Worm[V])
    inline def wormAsAtomic: AtomicReference[AnyRef] = worm

  extension [V](worm: kse.flow.Worm[V])

    inline def setIfEmpty(v: => V): Boolean =
      var old = worm.wormAsAtomic.get()
      if old eq Worm.notSetSentinel then
        worm.wormAsAtomic.compareAndSet(old, v.asInstanceOf[AnyRef])
      else false

    inline def get: V = worm.wormAsAtomic.get() match
      case x if x eq Worm.notSetSentinel => throw new java.lang.IllegalStateException("Retrieved value before being set")
      case x                             => x.asInstanceOf[V]

    inline def getOrSet(v: => V): V = worm.wormAsAtomic.get() match
      case x if x eq Worm.notSetSentinel =>
        setIfEmpty(v)
        get
      case x => x.asInstanceOf[V]
