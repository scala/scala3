object Test {
  type X[T] = ({ type F[_]; type R = F[T]})#R

  trait Monad[F[_]]
  type of[M[_[_]], T] = ({ type F[_]; type R = (given M[F]) => F[T]})#R
  def foo(a: of[Monad, String]) = ???
}
