class Foo[F <: Foo[F]]
class Bar extends Foo[Bar]

class A {
  def foo[T <: Foo[T], U >: Foo[T] <: T](x: T): T = x
  foo(new Bar) // error
}
