trait Foo[F[_]]

trait Bar[F[_]] extends Foo[F]
trait Baz[F[_]] extends Foo[F]

case class Applied[F[_], A](a: F[A])


object Applied extends AppliedLowPrio {
  implicit def barApplied[F[_]: Baz]: Baz[({ type L[X] = Applied[F, X] })#L] = ???
}

trait AppliedLowPrio {
  implicit def bazApplied[F[_]: Foo]: Foo[({ type L[X] = Applied[F, X] })#L] = ???
}


object Test {
  def test[F[_]](implicit bar: Bar[F], baz: Baz[F]) = implicitly[Foo[({ type L[X] = Applied[F, X] })#L]]
}
