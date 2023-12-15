trait Foo {
  class A

  class B {
    foo(10)
  }

  def foo(x: Int) = 5 + x
}


class Bar extends Foo {
  val a: A = new A         // OK
  val b = new B

  override def foo(x: Int) = x + id
  val id = 100             // warn
}