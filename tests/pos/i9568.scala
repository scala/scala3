trait Monad[F[_]] {
  def foo[A](fa: F[A]): Unit = {}
}

class Bla[F[_], A]

object Test1 {
  type Id[A] = A

  val bla: Bla[Id, Unit] = ???
  implicit def blaMonad[F[_]: Monad, S]: Monad[({type L[X] = Bla[F, X]})#L] = ???
  implicit def idMonad: Monad[Id] = ???

  blaMonad.foo(bla) // does not diverge
}

object Test2 {
  type Id[A] = A

  val bla: Bla[Id, Unit] = ???
  implicit def blaMonad[F[_], S](implicit ev: => Monad[F]): Monad[({type L[X] = Bla[F, X]})#L] = ???
  implicit def idMonad: Monad[Id] = ???

  blaMonad.foo(bla) // does not diverge
}
