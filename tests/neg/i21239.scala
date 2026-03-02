// 2
// A more minimised reproduction
package lib

import java.util.concurrent.atomic.AtomicReference

opaque type Worm[V] = AtomicReference[AnyRef]
object Worm:
  extension [V](worm: Worm[V])
    inline def wormAsAtomic: AtomicReference[AnyRef] = worm

  extension [V](worm: lib.Worm[V])
    def get: V  = worm.wormAsAtomic.get().asInstanceOf[V]
    def get2: V = get // error
