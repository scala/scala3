trait Apply[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]
    def map2[B, Z](fb: F[B])(f: (A, B) => Z): F[Z] = ???

  private case class IsMap[T <: Tuple](value: Tuple.Map[T, F])
  private inline def tupledGeneric[T <: Tuple](tuple: Tuple.Map[T, F]): F[T] =
    inline IsMap(tuple) match
      case t: IsMap[h *: EmptyTuple] => t.value.head.map(_ *: EmptyTuple)
      case t: IsMap[h *: t] =>
        val head = t.value.head
        val tail = tupledGeneric(t.value.tail)
        head.map2(tail)(_ *: _)

trait Monad[F[_]] extends Apply[F]:
  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]
  extension [A](fa: F[A]) override def map[B](f: A => B): F[B] = ???

opaque type Kleisli[F[_], A, B] = A => F[B]
given [F[_], A](using F: Monad[F]): Monad[[B] =>> Kleisli[F, A, B]] with
  extension [B](k: Kleisli[F, A, B])
    def flatMap[C](f: B => Kleisli[F, A, C]) = ???