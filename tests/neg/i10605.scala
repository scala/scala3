def xa[A, B, X, Y](f: X => ((A, B) ?=> Y)) =
  (z: X) => (a: A, b: B) => f(z)(using a, b)

def superxa1(using String, Int): Nothing = ???

def main =
  xa(Function.const(superxa1)(_: Int)) // error
