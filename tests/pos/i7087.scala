type Foo

type G[A]

type F[T] = T match {
  case G[a] => String
}

given A {
  def (tup: T) g[T] given (Foo: F[T]) = ???
}

def f(x: G[Int]) given (Foo: String) = x.g