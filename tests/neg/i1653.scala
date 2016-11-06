trait Foo {
  def foo() = new Unit with Foo  // error
}
