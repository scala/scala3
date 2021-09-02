enum SUB[-A, +B]:
  case Ev[X]() extends SUB[X, X]

trait Tag { type T }

def f(p: Tag, e: SUB[Int, p.T]): p.T = e match
  case SUB.Ev() => 0

def g(p: Tag, q: Tag, e: SUB[p.T, q.T]) = e match
  case SUB.Ev() =>
    // p.T <: q.T
    (??? : p.T) : q.T

def h1[Q](p: Tag, e: SUB[p.T, Q]) = e match
  case SUB.Ev() =>
    (??? : p.T) : Q

def h2[P](q: Tag, e: SUB[P, q.T]) = e match
  case SUB.Ev() =>
    (??? : P) : q.T
