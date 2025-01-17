object Structures:

  trait Functor[F[_]]:
    extension [A](fa: F[A])
      def map[B](f: A => B): F[B]
      def as[B](b: B): F[B] = map(_ => b)
      def void: F[Unit] = as(())

  trait Applicative[F[_]] extends Functor[F]:
    def pure[A](a: A): F[A]
    def unit: F[Unit] = pure(())
    extension[A](fa: F[A])
      def map2[B, C](fb: F[B], f: (A, B) => C): F[C]
      def map[B](f: A => B): F[B] =
        fa.map2(unit, (a, _) => f(a))

  trait Monad[F[_]] extends Applicative[F]:
    extension[A](fa: F[A])
      def flatMap[B](f: A => F[B]): F[B]
      override def map[B](f: A => B): F[B] =
        flatMap(a => pure(f(a)))
      def map2[B, C](fb: F[B], f: (A, B) => C): F[C] =
        flatMap(a => fb.map(b => f(a, b)))

  given Monad[List]:
    def pure[A](a: A) = List(a)
    extension[A](fa: List[A])
      def flatMap[B](f: A => List[B]) = fa.flatMap(f)

  given Monad[Option]:
    def pure[A](a: A) = Some(a)
    extension[A](fa: Option[A])
      def flatMap[B](f: A => Option[B]) = fa.flatMap(f)


  opaque type Kleisli[F[_], A, B] = A => F[B]

  extension [F[_], A, B](k: Kleisli[F, A, B])
    def apply(a: A): F[B] = k(a)

  object Kleisli:
    def apply[F[_], A, B](f: A => F[B]): Kleisli[F, A, B] = f

  given [F[_], A] => (F: Monad[F]) => Monad[[B] =>> Kleisli[F, A, B]]:
    def pure[B](b: B) = Kleisli(_ => F.pure(b))
    extension[B](k: Kleisli[F, A, B])
      def flatMap[C](f: B => Kleisli[F, A, C]) =
        a => k(a).flatMap(b => f(b)(a))

end Structures

@main def run =
  import Structures.{*, given}
  println(List(1, 2, 3).map2(List(4, 5, 6), (_, _)))

  val p: Kleisli[Option, Int, Int] = Kleisli((x: Int) => if x % 2 == 0 then Some(x) else None)
  val q: Kleisli[Option, Int, Int] = summon[Applicative[[B] =>> Kleisli[Option, Int, B]]].pure(20)
  println(p.map2(q, _ + _)(42))

