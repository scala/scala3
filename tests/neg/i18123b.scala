// Minimized version of `tests/pos/i18123.scala` to test #24425.

extension (x: String)
  transparent inline def rep(min: Int = 0): String = ???

def y: String = ???

def z = y.rep().toUpperCase // error
