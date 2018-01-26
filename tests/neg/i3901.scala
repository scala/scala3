object Crash {
  def f(cond: => Boolean): cond.type = ???
  f(true)
}
