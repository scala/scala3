inline def f(inline x: Int): Unit = {
  inline val twice = x + x
  inline val thrice = twice + x
  val res = thrice
  res
}

def test: Unit = f(3)
