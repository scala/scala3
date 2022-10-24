package covtest

class SomeFunctions:
  def f(x: Int) = ()
  def g() = 0
  def c = SomeFunctions()

  def test = c.f(g())
