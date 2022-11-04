abstract class Foo[+A]:
  def head: A
  def foo[T](xs: Foo[T]): T = xs match
    case xs: Foo[u] => xs.head: u
