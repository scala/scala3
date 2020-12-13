trait Applicative[F[_]]

def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]) = ???

object Test {
  implicit def eitherApplicative[A]: Applicative[[X] =>> Either[A, X]] = ???

  // Used to fail looking for `Applicative[[X] =>> Right[Nothing, X] | Left[Int, X]]`
  traverse(List(1, 2))(i => if (true) Right(i) else Left(i))
}
