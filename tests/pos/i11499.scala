trait Functor[F[_]]

object data {

  type OptionT[F[_], A] = F[Option[A]]

  def fold[F[_], A, B](value: OptionT[F, A])(f: Functor[F]): F[B] = ???

  def cata[F[_], A, B](value: OptionT[F, A])(f: Functor[F]): F[B] =
    fold(value)(f) // error
}
