package hk

trait Monad[M[_]] {
  def pure[A](a: A): M[A] = ???
  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B] = ???
}

class EitherMonad[T] extends Monad[[E] =>> Either[T, E]] {
}

type MapKV = [K] =>> [V] =>> Map[K,V]

type MapV = [_] =>> [V] =>> Map[String, V]

type MapEither = [K] =>> [L] =>> [R] =>> Map[K, Either[L, R]]

type Id[A] = A
