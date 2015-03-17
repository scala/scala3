trait C {
  def p: Any
}

trait A { self: C { def p: String } =>
  def f(b: => Unit): Unit = {}
  f { p }
}
