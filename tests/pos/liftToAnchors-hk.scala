class Foo[F[_], F2[X] >: F[X]] {
  def foo[A](using F[A] <:< F2[A]): Unit = {}
  foo
}
