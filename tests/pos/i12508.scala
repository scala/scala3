class Test {
  inline def test(fun: Any): Any = ???
  test {
    class Foo[X]:
      def x: X = ???
      def foo: Unit = this.x.toString
  }
}