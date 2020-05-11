trait Foo[F[_]]

trait Top {
  type F[X]
  def foo: Foo[F]
}

trait Left extends Top

trait Right extends Top {
  trait F[X]
  def foo: Foo[F] = new Foo[F] {}
}

class Bottom extends Left with Right