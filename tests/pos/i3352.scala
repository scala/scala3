class Test {
  class Foo {
    def bar(x: String): Int = 1
  }

  implicit class FooOps(foo: Foo) {
    def bar(x: Int, y: Int = 2): Int = 2 // compiles with no default argument
  }

  def test(foo: Foo): Unit = {
    foo.bar(1)
  }
}
