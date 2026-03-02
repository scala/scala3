// 3
// One way to fix the issue, using the
// "universal function call syntax"
// (to borrow from what Rust calls the syntax to
// disambiguate which trait's method is intended.)
import java.util.concurrent.atomic.AtomicReference

package lib:
  opaque type Worm[V] = AtomicReference[AnyRef]
  object Worm:
    extension [V](worm: Worm[V])
      def get: V  = worm.get().asInstanceOf[V]
      def get2: V = Worm.get(worm)

package test:
  import lib.Worm
  object Test:
    def usage(worm: Worm[String]): String = worm.get2
