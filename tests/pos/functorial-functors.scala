class Common:

  trait Functor:
    type F[X]
    extension [A](x: F[A]) def map[B](f: A => B): F[B]

  trait Monad extends Functor:
    extension [A](x: F[A])
      def flatMap[B](f: A => F[B]): F[B]
      def map[B](f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
end Common

