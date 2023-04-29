package pkg

trait Foo1[A]
trait Foo2[A] extends Foo1[A]

trait Bar[F[_]]
object Bar {
  implicit val bar: Bar[pkg.Foo2] = ???
}

trait Qux
object Qux {
  implicit def qux[F[_]](implicit bar: Bar[F]): F[Qux] = ???
}