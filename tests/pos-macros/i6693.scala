package towers.computes

import quoted._

sealed abstract class Computes[T]

object Computes {

  opaque type Opaque[T] = Int

  implicit class ComputesApplication1[T](using s: Scope)(fn : Computes[Opaque[T]])(using s.Type[T]) {
    def apply[A](arg1 : Computes[A]) : Computes[T] = ???
  }

  def let[V, T](using s: Scope)(value : Computes[V], body : Computes[Opaque[T]])(using s.Type[T]): Computes[T] = body(value)
}
