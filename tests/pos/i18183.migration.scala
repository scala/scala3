//> using options -source:3.0-migration

// A not-fully-minimal reproduction of the CI failure in http4s
// While implementing the fix to name "shadowing" in implicit lookup.

import scala.util.control.NoStackTrace

final case class EitherT[F[_], A, B](value: F[Either[A, B]]) {
  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): EitherT[F, A, D] = ???
}

trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
}
trait Monad[F[_]] extends Applicative[F]
trait Async[F[_]] extends Monad[F]

final class Request[+F[_]]

final case class RequestCookie(name: String, content: String)

final class CSRF2[F[_], G[_]](implicit F: Async[F]) { self =>
  import CSRF2._

  def signToken[M[_]](rawToken: String)(implicit F: Async[M]): M[CSRFToken] = ???

  def refreshedToken[M[_]](implicit F: Async[M]): EitherT[M, CSRFCheckFailed, CSRFToken] =
    EitherT(extractRaw("")).semiflatMap(signToken[M])

  def extractRaw[M[_]: Async](rawToken: String): M[Either[CSRFCheckFailed, String]] = ???
}

object CSRF2 {
  type CSRFToken

  case object CSRFCheckFailed extends Exception("CSRF Check failed") with NoStackTrace
  type CSRFCheckFailed = CSRFCheckFailed.type
}
