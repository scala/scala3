import annotation.retains
object Test:

  def f[A <: Matchable @retains(caps.*)](x: A): Matchable = x // error

