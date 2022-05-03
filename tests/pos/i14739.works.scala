abstract class Foo[T]:
  def head: T
  def foo(xs: Foo[T]): T = xs match
    case xs: Foo[u] => xs.head: u
