//> using options -Werror

infix enum Extends[A, B]:
  case Ev[B, A <: B]() extends (A Extends B)
