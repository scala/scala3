trait Monoid[A]
trait Semigroup[A]
trait Applicative[F[_]]

trait OptionT[F[_], A]
trait EitherT[F[_], A, B]
trait IorT[F[_], A, B]
trait WriterT[F[_], L, V]
trait Kleisli[F[_], A, B]

final class ApplicativeIdOps[A](private val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = ???
}

object ApplicativeSyntax {
  implicit final def syntaxApplicativeId[A](a: A): ApplicativeIdOps[A] = new ApplicativeIdOps[A](a)
}

trait Sync[F[_]]

object Sync {
  implicit def syncForOptionT[F[_]](implicit F0: Sync[F]): Sync[[X] =>> OptionT[F, X]] = ???
  implicit def syncForEitherT[F[_], E](implicit F0: Sync[F]): Sync[[X] =>> EitherT[F, E, X]] = ???
  implicit def syncForIorT[F[_], L](implicit F0: Sync[F], L0: Semigroup[L]): Sync[[X] =>> IorT[F, L, X]] = ???
  implicit def syncForWriterT[F[_], L](implicit F0: Sync[F], L0: Monoid[L]): Sync[[X] =>> WriterT[F, L, X]] = ???
  implicit def syncForKleisli[F[_], R](implicit F0: Sync[F]): Sync[[X] =>> Kleisli[F, R, X]] = ???
}

trait Async[F[_]] extends Sync[F]

object Async {
  implicit def asyncForOptionT[F[_]](implicit F0: Async[F]): Async[[X] =>> OptionT[F, X]] = ???
  implicit def asyncForEitherT[F[_], E](implicit F0: Async[F]): Async[[X] =>> EitherT[F, E, X]] = ???
  implicit def asyncForIorT[F[_], L](implicit F0: Async[F], L0: Semigroup[L]): Async[[X] =>> IorT[F, L, X]] = ???
  implicit def asyncForWriterT[F[_], L](implicit F0: Async[F], L0: Monoid[L]): Async[[X] =>> WriterT[F, L, X]] = ???
  implicit def asyncForKleisli[F[_], R](implicit F0: Async[F]): Async[[X] =>> Kleisli[F, R, X]] = ???
}

trait Concurrent[F[_], E] extends Applicative[F]

trait Ref[F[_], A]

object Ref {
  trait Make[F[_]]
  object Make extends MakeInstances

  trait MakeInstances extends MakeLowPriorityInstances {
    implicit def concurrentInstance[F[_]](implicit F: Concurrent[F, _]): Make[F] = ???
  }

  trait MakeLowPriorityInstances {
    implicit def syncInstance[F[_]](implicit F: Sync[F]): Make[F] = ???
  }

  def of[F[_], A](a: A)(implicit mk: Make[F]): F[Ref[F, A]] = ???
}


class Resource[F[_], A] {
  import ApplicativeSyntax.*

  implicit def asyncForResource[F[_]](implicit F0: Async[F]): Async[[X] =>> Resource[F, X]] = ???

  def parZip(implicit F: Concurrent[F, Throwable]) = {
    Ref.of /*[F, (F[Unit], F[Unit])]*/ (().pure[F] -> ().pure[F])
    ()
  }
}