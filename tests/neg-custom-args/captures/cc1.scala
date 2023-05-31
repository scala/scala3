import annotation.retains
object Test:

  def f[A <: Matchable @retains(caps.cap)](x: A): Matchable = x // error

