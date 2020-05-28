inline def f(inline x: Int): Unit = {
  inline val twice = x + x
}

def test: Unit = f(3)
