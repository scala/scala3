//> using options -language:experimental.modularity -source future
trait A[X]:
  type Self = X

object Test2:
  def foo[X: A as x](a: x.Self) = ???

  def bar[X: A as x](a: Int) = ???

  def baz[X: A as x](a: Int)(using String) = ???
