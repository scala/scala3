object Test0 {

  def f(x: Int) = {
    transparent def g(x: Int) = x match {
      case 0 => 0
    }
    g(0)
    transparent val Y = 0
    g(Y)

    transparent def h(x: Int) = x match {
      case Y => 0
    }
    h(0)
  }

  f(0)

}

object Test1 {

  erased transparent def f(x: Int) = {
    erased transparent def g(x: Int) = x match {
      case 0 => 0
    }
    g(0)
    transparent val Y = 0
    g(Y)

    transparent def h(x: Int) = x match {
      case Y => 0
    }
    h(0)
  }

  f(0)

}