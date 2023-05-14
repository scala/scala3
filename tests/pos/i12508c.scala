def fun(a: Any, b: Any = 2): Any = ???

def test =
  fun(
    b = println(1),
    a = {
      enum Option[+X]:
        case Some(x: X)
        case None
      if ??? then Option.Some(1) else Option.None
    }
  )