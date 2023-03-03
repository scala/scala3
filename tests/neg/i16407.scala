trait X { self: Y =>
  f(g()) // error // error
}
trait Y { self: Z =>
  type B = A
  def f(a: B): Unit = ()
  def g(): A = ???
}
trait Z {
  type A
}
