//> using options -source:future

trait Foo[T <: Foo[T]] {
  type I <: Foo[I]
}

trait Bar[T <: Foo[T]] extends Foo[T] { // was error: cyclic
  self: T =>
}
