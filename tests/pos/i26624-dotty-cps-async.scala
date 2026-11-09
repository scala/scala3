trait Ctx[F[_]]
trait TryCtx[F[_]] extends Ctx[F]

trait Monad[F[_]]:
  type Context <: Ctx[F]
  def pure[T](t: T): F[T]
  def apply[T](op: Context => F[T]): F[T]

object Monad:
  type Aux[F[_], C <: Ctx[F]] = Monad[F] { type Context = C }

trait TryMonad[F[_]] extends Monad[F]:
  type Context <: TryCtx[F]

class TryBody[F[_]] extends TryCtx[F]

trait TryInstance[F[_]] extends TryMonad[F]:
  type Context = TryBody[F]
  def apply[T](op: Context => F[T]): F[T] = op(TryBody())

class InferAsyncArg[F[_], C <: Ctx[F]](using val am: Monad.Aux[F, C]):
  transparent inline def apply[T](inline expr: C ?=> T): F[T] =
    am.apply(ctx => am.pure(expr(using ctx)))

transparent inline def async[F[_]](using am: Monad[F]) =
  new InferAsyncArg(using am)

case class IO[A](value: A)

implicit def ioTryMonad: TryMonad[IO] = new TryMonad[IO] with TryInstance[IO]:
  def pure[T](t: T) = IO(t)

def program: IO[Int] = async[IO] {
  1 + 1
}
