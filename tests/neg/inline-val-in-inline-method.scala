inline def f(inline x: Int): Unit =
  inline val b = x
  val c: b.type = b

def test =
  f(1)
  def a = 1
  f(a) // error: inline value must have a literal constant type
