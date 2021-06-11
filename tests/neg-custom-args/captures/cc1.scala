object Test:

  def f[A <: Any retains *](x: A): Any = x // error

