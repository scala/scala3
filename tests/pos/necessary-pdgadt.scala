/* N <: M */
trait M
trait N extends M

enum SUB[-A, +B]:
  case Ev[X]() extends SUB[X, X]

trait P { type T }

def f(p: P, e: SUB[p.T, N | M]) = e match
  case SUB.Ev() =>
    // p.T <: M
    (??? : p.T) : M

