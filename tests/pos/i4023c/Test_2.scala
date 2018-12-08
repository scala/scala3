object Test {
  Macro.ff(3)

  def f[T](x: T) = {
    Macro.ff(x)
  }
}
