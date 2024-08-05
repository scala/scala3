
trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B] = ???
trait Monad[F[_]] extends Functor[F]
trait MonadError[F[_], E] extends Monad[F]:
  def raiseError[A](e: E): F[A]
trait Temporal[F[_]] extends MonadError[F, Throwable]

trait FunctorOps[F[_], A]:
  def map[B](f: A => B): F[B] = ???
implicit def toFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): FunctorOps[F, A] = ???

class ContextBounds[F[_]: Temporal](using err: MonadError[F, Throwable]):
  def useCase = err.raiseError(new RuntimeException())
  val bool: F[Boolean] = ???
  def fails = toFunctorOps(bool).map(_ => ())  // warns under -source:3.5, // error under -source:3.6

class UsingArguments[F[_]](using Temporal[F])(using err: MonadError[F, Throwable]):
  def useCase = err.raiseError(new RuntimeException())
  val bool: F[Boolean] = ???
  def works = toFunctorOps(bool).map(_ => ()) // warns under -source:3.5

