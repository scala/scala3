trait Foo {
  @scala.annotation.init
  class A

  class B {
    foo(10)
  }

  def foo(x: Int) = 5 + x
}


class Bar extends Foo {
  val a: A = new A         // OK
  println(a)               // OK
  val b = new B            // error

  override def foo(x: Int) = x + id
  val id = 100
}