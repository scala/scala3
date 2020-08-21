sealed abstract class Resource[+F[_], +A] {
  import Resource.{Allocate, Bind, Suspend}

  def loop[G[x] >: F[x], B](current: Resource[G, Any]): G[B] =
    current match {
      case Allocate(r) => ???
      case Bind(s, fs) => ???
      case Suspend(r) => ???
    }
}

object Resource {

  final case class Allocate[F[_], A](resource: F[A])
      extends Resource[F, A]

  final case class Bind[F[_], S, +A](source: Resource[F, S], fs: S => Resource[F, A])
      extends Resource[F, A]

  final case class Suspend[F[_], A](resource: F[Resource[F, A]]) extends Resource[F, A]

}
