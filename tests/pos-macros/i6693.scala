package towers.computes

import quoted.*

sealed abstract class Computes[T]

object Computes {

  opaque type Opaque[T] = Int

  implicit class ComputesApplication1[T : Type](fn : Computes[Opaque[T]]) {
    def apply[A](arg1 : Computes[A]) : Computes[T] = ???
  }

  def let[V, T : Type](value : Computes[V], body : Computes[Opaque[T]]) : Computes[T] = body(value)
}
