type Foo

type G[A]

type F[T] = T match {
  case G[a] => String
}

extension on [T](tup: T) {
  def g(using Foo: F[T]) = ???
}

def f(x: G[Int])(using Foo: String) = x.g