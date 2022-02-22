class Test {
  inline def test(fun: Any): Any = ???
  test {
    class Foo[X]:
      def x: X = ???
      def foo: Unit = this.x.toString
  }
}
class Test2 {
  inline def test(fun: => Any): Any = fun
  test {
    case class Pair[X, Y](
      x: X,
      y: Y,
    )
  }
}