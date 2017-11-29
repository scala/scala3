class Foo {
  class A

  type Bla[X <: A] = X
}

class Bar extends Foo {
  val y: Bla[A] = ???
}
