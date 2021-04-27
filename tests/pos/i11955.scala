object Hello {

  sealed abstract class X[+A] {
    type This[+A] <: X[A]
    def asThis: This[A]
  }

  class Y[+A] extends X[A] {
    override type This[+AA] = Y[AA]
    override def asThis: This[A] = this
  }

  def hackBackToSelf[F[+u] <: X[u], A](f: F[Any])(f2: f.This[A]): F[A] =
    f2.asInstanceOf[F[A]]

  case class G[F[+u] <: X[u], A](wrapped: F[A]) {

    def mapF[F2[+u] <: X[u]](f: F[A] => F2[A]): G[F2, A] =
      G[F2, A](f(wrapped))

    def test_ko_1: G[F, A] = mapF(ct => hackBackToSelf(ct)(ct.asThis)) // error
    def test_ko_2: G[F, A] = mapF[F](ct => hackBackToSelf(ct)(ct.asThis)) // error
    def test_ok  : G[F, A] = mapF(ct => hackBackToSelf[F, A](ct)(ct.asThis)) // ok
  }
}