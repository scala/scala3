object Crash {
  def f(cond: => Boolean): cond.type = ???  // error: cond is not stable
  f(true)
}
