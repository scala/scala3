abstract class Foo[T] {
  type Species
  def foo(s: Species): Nothing = ???
}

class Test {
def species[T] = {
  class FooT extends Foo[T] {
    type Species = FooT
  }
  new FooT()
}
}