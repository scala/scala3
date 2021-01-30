trait Type { type T }
object Type { implicit def Type[S]: Type { type T = S } = new Type { type T = S } }

trait Type1 { type T[_] }
object Type1 { implicit def Type1[S[_]]: Type1 { type T[A] = S[A] } = new Type1 { type T[A] = S[A] } }

trait Functor[F[_]] { def map[A,B](x: F[A])(f: A => B): F[B] }
object Functor { implicit object listFun extends Functor[List] { def map[A,B](ls: List[A])(f: A => B) = ls.map(f) } }

val map: (A:Type,B:Type,F:Type1) ?=>  (Functor[F.T]) ?=> (F.T[A.T]) => (A.T => B.T) => F.T[B.T] =
  fun ?=> x => f => fun.map(x)(f) // error