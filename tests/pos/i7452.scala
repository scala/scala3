trait Abstract[F[_], A] {
  type Out[G[_]]
}
object Abstract {
  type Aux[F[_], A, Out0[_[_]]] = Abstract[F, A] { type Out[G[_]] = Out0[G] }

  implicit def valueAbstract[F[_], A]: Aux[F, F[A], [G[_]] =>> G[A]] = new Abstract[F, F[A]] {
    type Out[G[_]] = G[A]
  }

  implicit def hkdAbstract[F[_], A[_[_]]]: Aux[F, A[F], [G[_]] =>> A[G]] = new Abstract[F, A[F]] {
    type Out[G[_]] = A[G]
  }
}

case class StringF[F[_]](s: F[String])

trait Q[M[_[_]]] {
  def map[A, M2[_[_]]](f: M[Option] => A)(implicit abs: Abstract.Aux[Option, A, M2]): Q[M2]
}

val q: Q[[F[_]] =>> F[String]] = ???
val q2 = q.map(s => s)   // was an error, now OK
val q3 = q.map(s => StringF(s))
