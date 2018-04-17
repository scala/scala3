class M

class A extends M
class B extends M

class Foo[T](x: T) {
  def foo[S >: T](other: S): Foo[S] = ???
}

object C {
  def xy() = {
    val x: Foo[A] = new Foo(new A)
    x.foo(new B)
  }
}
