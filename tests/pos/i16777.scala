//> using options -Xkind-projector:underscores

sealed abstract class Free[+S[_, _], +E, +A] {
  @inline final def flatMap[S1[e, a] >: S[e, a], B, E1 >: E](fun: A => Free[S1, E1, B]): Free[S1, E1, B] = Free.FlatMapped[S1, E, E1, A, B](this, fun)
  @inline final def map[B](fun: A => B): Free[S, E, B] = flatMap(a => Free.pure[S, B](fun(a)))
  @inline final def as[B](as: => B): Free[S, E, B] = map(_ => as)
  @inline final def *>[S1[e, a] >: S[e, a], B, E1 >: E](sc: Free[S1, E1, B]): Free[S1, E1, B] = flatMap(_ => sc)
  @inline final def <*[S1[e, a] >: S[e, a], B, E1 >: E](sc: Free[S1, E1, B]): Free[S1, E1, A] = flatMap(r => sc.as(r))

  @inline final def void: Free[S, E, Unit] = map(_ => ())

  // FIXME: Scala 3.1.4 bug: false unexhaustive match warning
  /// @nowarn("msg=pattern case: Free.FlatMapped")
  @inline final def foldMap[S1[e, a] >: S[e, a], G[+_, +_]](transform: S1 ~>> G)(implicit G: Monad2[G]): G[E, A] = {
    this match {
      case Free.Pure(a) => G.pure(a)
      case Free.Suspend(a) => transform.apply(a)
      case Free.FlatMapped(sub, cont) =>
        sub match {
          case Free.FlatMapped(sub2, cont2) => sub2.flatMap(a => cont2(a).flatMap(cont)).foldMap(transform)
          case another => G.flatMap(another.foldMap(transform))(cont(_).foldMap(transform))
        }
    }
  }
}

trait ~>>[-F[_, _], +G[_, _]] {
  def apply[E, A](f: F[E, A]): G[E, A]
}

object Free {
  @inline def pure[S[_, _], A](a: A): Free[S, Nothing, A] = Pure(a)
  @inline def lift[S[_, _], E, A](s: S[E, A]): Free[S, E, A] = Suspend(s)

  final case class Pure[S[_, _], A](a: A) extends Free[S, Nothing, A] {
    override def toString: String = s"Pure:[$a]"
  }
  final case class Suspend[S[_, _], E, A](a: S[E, A]) extends Free[S, E, A] {
    override def toString: String = s"Suspend:[$a]"
  }
  final case class FlatMapped[S[_, _], E, E1 >: E, A, B](sub: Free[S, E, A], cont: A => Free[S, E1, B]) extends Free[S, E1, B] {
    override def toString: String = s"FlatMapped:[sub=$sub]"
  }
}

type Monad2[F[+_, +_]] = Monad3[λ[(`-R`, `+E`, `+A`) => F[E, A]]]

trait Monad3[F[-_, +_, +_]] {
  def flatMap[R, E, A, B](r: F[R, E, A])(f: A => F[R, E, B]): F[R, E, B]
  def flatten[R, E, A](r: F[R, E, F[R, E, A]]): F[R, E, A] = flatMap(r)(identity)
  def pure[A](a: A): F[Any, Nothing, A]
}
