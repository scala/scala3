package dotty.generic

trait Representable1[A] {
  type Repr[t] // <: SumK[t] | ProdK[t] ← when we stop cross compiling

  def to[T](a: A): Repr[T]
  def from[T](r: Repr[T]): A
}

object Representable1 {
  def apply[A](implicit r: Representable1[A]): r.type = r
}
