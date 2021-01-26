trait Foo[T]
object Foo:
  given [T]: Foo[Tuple1[T]] with {}
  given [T, U]: Foo[(T, U)] with {}
  given [T, U, V]: Foo[(T, U, V)] with {}