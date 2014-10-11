trait A { self: Any { def p: Any } =>
  def f(b: => Unit): Unit = {}
  f { p } // error: cannot access member 'p' from structural type
}
