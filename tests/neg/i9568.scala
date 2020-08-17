trait Monad[F[_]] {
  def foo[A](fa: F[A]): Unit = {}
}

class Bla[F[_], A]

object Test {
  type Id[A] = A

  val bla: Bla[Id, Unit] = ???
  implicit def blaMonad[F[_], S](implicit ev: => Monad[F]): Monad[({type L[X] = Bla[F, X]})#L] = ???

  blaMonad.foo(bla) // error: diverges
}