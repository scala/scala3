package dotty.generic

trait Representable[A] {
  type Repr // <: Sum | Prod ← when we stop cross compiling

  def to(a: A): Repr
  def from(r: Repr): A
}

object Representable {
  def apply[A](implicit r: Representable[A]): r.type = r
}
