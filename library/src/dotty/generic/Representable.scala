package dotty.generic

trait Representable[A] {
  type Repr[t] // <: Sum[t] | Prod[t] ← for when we stop cross compiling

  // def to[T](a: A): Repr[T]
  // def from[T](r: Repr[T]): A
}
