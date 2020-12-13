trait Monad[F[_]] {
  def foo[A](fa: F[A]): Unit = {}
}

class Bla[F[_], A]

object Test {
  type Id[A] = A

  val bla: Bla[Id, Unit] = ???
  implicit def blaMonad[F[_]: Monad, S]: Monad[({type L[X] = Bla[F, X]})#L] = ???

  blaMonad.foo(bla) // error: divergence
}