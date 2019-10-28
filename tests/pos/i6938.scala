trait Foo[T]
object Foo with
  given [T]: Foo[Tuple1[T]]
  given [T, U]: Foo[(T, U)]
  given [T, U, V]: Foo[(T, U, V)]