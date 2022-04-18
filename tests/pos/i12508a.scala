def fun(a: Any, b: Any = 2): Any = ???
def test =
  fun(
    b = println(1),
    a = {
      class Foo[X]:
        def x: X = ???
        def foo: Unit = this.x.toString
        locally {
          def x: X = ???
          println(x.toString)
        }
    }
  )