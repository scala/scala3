package i7648

class IO[+A]

trait Functor[F[_]]
trait Monad[F[_]] extends Functor[F]

class Stream[+F[_], +A] {
  def take[F1[x] >: F[x]](n: Int)(implicit f: Functor[F1]): Stream[F1, A] = {
    this
  }
}

object Test with

  implicit val ioMonad: Monad[IO] = null

  val x = new Stream[IO, Int].take[IO](10)
