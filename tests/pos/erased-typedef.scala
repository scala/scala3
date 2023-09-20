//> using options -experimental -language:experimental.erasedDefinitions

trait Monadless[Monad[_]] {

  type M[T] = Monad[T]

  def lift[T](body: T): Monad[T] =  ???

  def unlift[T](m: M[T]): T = ???
}