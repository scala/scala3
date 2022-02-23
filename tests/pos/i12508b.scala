def fun(a: Any, b: Any = 2): Any = ???
def test =
  fun(
    b = println(1),
    a = {
      sealed class Foo[X]:
        def x: X = ???
        def foo: Unit = this.x.toString
      class Bar extends Foo[String]
      class Baz[Y] extends Foo[Y]
      if ??? then Bar() else Baz[Int]
    }
  )