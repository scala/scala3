enum SUB[-A, +B]:
  case Refl[X]() extends SUB[X, X]

def foo[C, A >: C <: C, B](e: SUB[B, A]) = e match
  case SUB.Refl() =>