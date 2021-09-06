trait P:
  case class A()
  type B

enum SUB[-A, +B]:
  case EQ[X]() extends SUB[X, X]

def f(p: P, e1: SUB[p.A, Int], e2: SUB[p.B, Int]) = e1 match
  case SUB.EQ() => e2 match
    case SUB.EQ() =>
      val t1: Int = ??? : p.A  // error
      val t2: Int = ??? : p.B
