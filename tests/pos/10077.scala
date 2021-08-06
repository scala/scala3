trait T[F[_[_]]]

type Inner[x] = [X[_]] =>> x match { case T[f] => f[X] }

trait Monad[F[_]]
type TMonad = T[Monad]

trait U[T0]:
  type T0_member = T0
  def f(x: Inner[T0][List]): Unit

class X extends U[T[Monad]]:
  def f(x: Inner[T0_member][List]): Unit = ???
