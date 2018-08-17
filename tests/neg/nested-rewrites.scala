object Test {

  rewrite def f1(x: Int) = {
    rewrite def g(x: Int) = x // error: implementation restriction: nested rewrite methods are not supported
    g(x)
  }

  rewrite def f2(x: Int) = {
    object o {
      rewrite def g(x: Int) = x // error: implementation restriction: nested rewrite methods are not supported
    }
    o.g(x)
  }
}
object Test0 {

  def f(x: Int) = {
    rewrite def g(x: Int) = rewrite x match {
      case 0 => 0
    }
    g(0)
    transparent val Y = 0
    g(Y)

    rewrite def h(x: Int) = rewrite x match {
      case Y => 0
    }
    h(0)
  }

  f(0)

}

object Test1 {

  erased rewrite def f(x: Int) = {
    erased rewrite def g(x: Int) = rewrite x match { // error: implementation restriction: nested rewrite methods are not supported
      case 0 => 0
    }
    g(0)
    transparent val Y = 0
    g(Y)

    rewrite def h(x: Int) = rewrite x match { // error: implementation restriction: nested rewrite methods are not supported
      case Y => 0
    }
    h(0)
  }

  f(0)

}