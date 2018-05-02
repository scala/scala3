trait Foo {
  @scala.annotation.filled
  class B {
    foo(10)                    // error
  }

  def foo(x: Int) = 5 + x
}