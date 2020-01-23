trait Foo[T]
object Foo:
  given [T] as Foo[Tuple1[T]]
  given [T, U] as Foo[(T, U)]
  given [T, U, V] as Foo[(T, U, V)]