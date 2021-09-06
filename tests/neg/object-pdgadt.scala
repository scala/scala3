object P:
  type T

enum SUB[-A, +B]:
  case EQ[X]() extends SUB[X, X]

def f(p: P.type, e: SUB[P.T, Int]) = p match
  case _: P.type =>
    val t0: Int = ??? : p.T  // error
