trait MyTC[F[_]]

object MyTC:
  implicit def forFunction1[A]: MyTC[[X] =>> A => X] = ???

final class Ops[F[_], A](private val fa: F[A]) extends AnyVal:
  def >>=[B](f: A => F[B]): F[B] = ???
  def >>[B](fb: => F[B]): F[B]   = ???

implicit def syntax[F[_], A](fa: F[A])(using MyTC[F]): Ops[F, A] =
  new Ops[F, A](fa)

trait Console[F[_]]:
  def println(s: String): F[Unit]

object Console:
  def apply[F[_]: Console]: Console[F] = summon

extension [A](a: A)
  def |>[B](f: A => B): B = f(a)

def render(s: String): String = ""

def loop[F[_]: {MyTC, Console}]: F[Unit] =
  Console[F].println("x")
    >>= render(_) |> Console[F].println // error
    >> loop[F]

object Test:
  Console[F].println("x") // error
    >>= render(_ /* Unit, expected String */) // error
