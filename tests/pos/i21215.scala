
trait FlatMap[F[_]]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = ???
  def ifM[B](ifTrue: => F[B], ifFalse: => F[B])(implicit F: FlatMap[F]): F[B] = ???
trait Monad[F[_]] extends FlatMap[F]
trait MonadError[F[_], E] extends Monad[F]:
  def raiseError[A](e: E): F[A]
trait Temporal[F[_]] extends MonadError[F, Throwable]

trait FlatMapOps[F[_], A]:
  def flatMap[B](f: A => F[B]): F[B] = ???
  def map[B](f: A => B): F[B] = ???
  def ifM[B](ifTrue: => F[B], ifFalse: => F[B])(implicit F: FlatMap[F]): F[B] = ???

implicit def toFlatMapOps[F[_], A](target: F[A])(implicit tc: FlatMap[F]): FlatMapOps[F, A] = ???

abstract class Ref[F[_], A]
object Ref:
  final class ApplyBuilders[F[_]]:
    def of[A](a: A): F[Ref[F, A]] = ???
  def apply[F[_]]: ApplyBuilders[F] = ???

trait DatabaseMetaData[F[_]]
class DatabaseMetaDataImpl[F[_]](
                                  statementClosed: Ref[F, Boolean],
                                  resultSetClosed: Ref[F, Boolean]
                                ) extends DatabaseMetaData[F]

trait LdbcConnection[F[_]]:
  def getMetaData(): F[DatabaseMetaData[F]]

class ConnectionImpl[F[_]: Temporal](using ev: MonadError[F, Throwable])
  extends LdbcConnection[F]:
  def isClosed(): F[Boolean] = ???
  override def getMetaData(): F[DatabaseMetaData[F]] =
    isClosed().ifM(
      ev.raiseError(???),
      (for
        statementClosed <- Ref[F].of[Boolean](false)
        resultSetClosed <- Ref[F].of[Boolean](false)
      yield DatabaseMetaDataImpl[F](
        statementClosed,
        resultSetClosed
      ))
    )
