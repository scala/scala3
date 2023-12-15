class Foo {
  class B {
    foo(10)
  }

  new B
  val a = 3    // warn

  def foo(x: Int) = a + x
}