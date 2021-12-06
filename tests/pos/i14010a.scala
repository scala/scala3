class Foo(top: => Int) {
  def foo: Any = new Foo(top) { }
}