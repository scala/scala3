object Test {

  inline def f1(x: Int) = {
    inline def g(x: Int) = x // error: implementation restriction: nested inline methods are not supported
    g(x)
  }

  inline def f2(x: Int) = {
    object o {
      inline def g(x: Int) = x // error: implementation restriction: nested inline methods are not supported
    }
    o.g(x)
  }
}
object Test0 {

  def f(x: Int) = {
    inline def g(x: Int) = inline x match {
      case 0 => 0
    }
    g(0)
    inline val Y = 0
    g(Y)

    inline def h(x: Int) = inline x match {
      case Y => 0
    }
    h(0)
  }

  f(0)

}

object Test1 {

  erased inline def f(x: Int) = {
    erased inline def g(x: Int) = inline x match { // error: implementation restriction: nested inline methods are not supported
      case 0 => 0
    }
    g(0)
    inline val Y = 0
    g(Y)

    inline def h(x: Int) = inline x match { // error: implementation restriction: nested inline methods are not supported
      case Y => 0
    }
    h(0)
  }

  f(0)

}