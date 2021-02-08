inline def f(inline thunk: Any): Unit = thunk
transparent inline def g(inline thunk: Any): Unit = thunk

def test: Unit =
  f {
    class C1(val i: Int)
    val c = C1(1)
    c.i
  }

  f {
    case class C2(i: Int)
    val c = C2(2)
    c.i
  }

  g {
    class C3(val i: Int)
    val c = C3(3)
    c.i
  }

  g {
    case class C4(i: Int)
    val c = C4(4)
    c.i
  }