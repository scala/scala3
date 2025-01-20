// 4
// An alternative way to fix it,
// defining the extension method externally,
// in a scope that doesn't see through
// the opaque type definition.
// The setup here also makes sure those extension
// are on the opaque type's companion object
// (via class extension), meaning that they continue
// to be in implicit scope (as enforced by the usage test)
import java.util.concurrent.atomic.AtomicReference

package lib:
  object Worms:
    opaque type Worm[V] = AtomicReference[AnyRef]
    object Worm extends WormOps:
      extension [V](worm: Worm[V])
        inline def wormAsAtomic: AtomicReference[AnyRef] = worm

  import Worms.Worm
  trait WormOps:
    extension [V](worm: Worm[V])
      def get: V  = worm.wormAsAtomic.get().asInstanceOf[V]
      def get2: V = get

package test:
  import lib.Worms.Worm
  object Test:
    def usage(worm: Worm[String]): String = worm.get2
