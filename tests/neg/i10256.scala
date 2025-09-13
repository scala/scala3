trait Foo[T <: Foo[T]] {
  type I <: Foo[I]
}

trait Bar[T <: Foo[T]] extends Foo[T] { // error: cyclic
  self: T =>
}
