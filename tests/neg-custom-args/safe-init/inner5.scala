trait Foo {
  @scala.annotation.init
  class B {
    foo(10)                    // error
  }

  def foo(x: Int) = 5 + x
}