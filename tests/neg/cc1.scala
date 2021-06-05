object Test:

  def f[A <: Any holds *](x: A): Any = x // error

