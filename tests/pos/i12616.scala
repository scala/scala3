class Foo:

  //object Bar
  val Bar = 22

  object Baz:
    def f(x: Any): Unit =
      x match
        case s: (Bar.type & x.type) =>