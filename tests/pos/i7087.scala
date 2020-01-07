type Foo

type G[A]

type F[T] = T match {
  case G[a] => String
}

extension of [T](tup: T) with {
  def g(given Foo: F[T]) = ???
}

def f(x: G[Int])(given Foo: String) = x.g