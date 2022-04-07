package x

trait CpsMonad[F[_]]:
  def pure[A](x:A): F[A]
  def flatMap[A,B](fa:F[A])(f: A=>F[B]): F[B]

abstract sealed class CpsStream[-F[_],+T]

case class Cons[F[_],T](head:T, tailFun: ()=>F[CpsStream[F,T]]) extends CpsStream[F,T]

case class Empty[F[_]]() extends CpsStream[F,Nothing]

def unfold[S,F[_]:CpsMonad,T](s0:S)(f:S => F[Option[(S,T)]]):F[CpsStream[F,T]] =
      summon[CpsMonad[F]].flatMap(f(s0)){
         case Some(s1,a) => Cons(a, () => unfold(s1,f))  // error (used to crash)
         case None => summon[CpsMonad[F]].pure(Empty[F]())
      }