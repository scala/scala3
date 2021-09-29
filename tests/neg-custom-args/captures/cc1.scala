object Test:

  def f[A <: Matchable @retains(*)](x: A): Matchable = x // error

