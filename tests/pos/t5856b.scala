class Test with
  def test = f("a" == s"a")
  inline def f(inline b: Boolean): Boolean = !b
