trait Applicative[F[_]]

trait FooT[F[_], A]
trait BarT[F[_], A]
trait QuxT[F[_], A]
trait BazT[F[_], A]
trait ZepT[F[_], A]
trait JazT[F[_], A]
trait LafT[F[_], A]
trait PogT[F[_], A]

trait Sync[F[_]]
object Sync {
  implicit def syncForFooT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> FooT[F, X]] = ???
  implicit def syncForBarT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> BarT[F, X]] = ???
  implicit def syncForQuxT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> QuxT[F, X]] = ???
  implicit def syncForBazT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> BazT[F, X]] = ???
  implicit def syncForZepT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> ZepT[F, X]] = ???
  implicit def syncForJazT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> JazT[F, X]] = ???
  // defining additional implicits beyond the 6 above seems to result in hang/OOM
  implicit def syncForLafT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> LafT[F, X]] = ???
  implicit def syncForPogT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> PogT[F, X]] = ???
}

trait Ref[F[_], A]
object Ref {
  trait Make[F[_]]
  object Make extends MakeInstances

  trait MakeInstances extends MakeLowPriorityInstances {
    implicit def applicativeInstance[F[_]](implicit F: Applicative[F]): Make[F] = ???
  }

  trait MakeLowPriorityInstances {
    implicit def syncInstance[F[_]](implicit F: Sync[F]): Make[F] = ???
  }

  def of[F[_], A](a: A)(implicit mk: Make[F]): F[Ref[F, A]] = ???
}

class Resource[F[_], A] {
  implicit def syncForResource[F[_]](implicit F0: Sync[F]): Sync[[X] =>> Resource[F, X]] = ???

  def foo(x: F[Unit])(implicit F: Applicative[F]) = {
    Ref.of /*[F, (F[Unit], F[Unit])]*/ ((x, x))
    ()
  }
}