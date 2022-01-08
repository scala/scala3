object Test:
  trait MyTypeclass[F[_]]
  def f[F[_]: MyTypeclass, U](t: F[U]) = ()

  type MyType[T] = String
  given MyTypeclass[MyType] = null

  val stream: Option[MyType[Int]] = null
  for
    keyStream <- stream
    x = 17
  yield f(keyStream)