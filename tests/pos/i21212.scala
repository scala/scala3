
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


object Minimization:

  trait A
  trait B extends A

  def test1(using a1: A)(using    b1: B) = summon[A] // picks (most general) a1
  def test2(using a2: A)(implicit b2: B) = summon[A] // picks (most general) a2, was ambiguous
  def test3(implicit       a3: A, b3: B) = summon[A] // picks (most specific) b3

end Minimization
