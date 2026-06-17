def xa[A, B, X, Y](f: X => ((A, B) ?=> Y)) =
  (z: X) => (a: A, b: B) => f(z)(using a, b)

def superxa1(using String, Int): Nothing = ???
def superxa2(using String, Int): Unit = ???

def main =
  xa(Function.const(superxa1)(_: Int)) // error
  xa(Function.const(superxa2)(_: Int)) // error