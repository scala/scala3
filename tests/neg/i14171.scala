object Test:
  trait MyTypeclass[F[_]]
  def f[F[_]: MyTypeclass, U](t: F[U]) = ()

  // using a non-singleton type makes it compile because `widenSingletons` dealiases
  type MyType[T] = Nil.type
  given MyTypeclass[MyType] = null

  val stream: Option[MyType[Int]] = null
  for
    keyStream <- stream
    x = 17 // commenting this line makes it compile
  yield f(keyStream) // error