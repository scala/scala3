object Test {

  class C { type T }

  def f(x: C, y: x.T): x.T = y // ok

  def g(y: x.T, x: C): x.T = y // error

  def h(x: x.T) = ??? // error

  def g(x: => C): x.T = ???  // error: x is not stable

}
