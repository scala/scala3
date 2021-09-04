trait P { type S; type T >: S }

enum SUB[-A, +B]:
  case EQ[X]() extends SUB[X, X]

def f(p: P, e: SUB[Int, p.S]): p.T = e match
  case SUB.EQ() => 42

