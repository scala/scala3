class Foo[T[_, _], F[_], A, B](val fa: T[F[A], F[B]])

object Test {
  def x[T[_, _]](tmab: T[Either[Int, String], Either[Int, Int]]) =
    new Foo(tmab)
}
