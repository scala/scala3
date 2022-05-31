type FAux[A0] = Object{type A = A0}

type F[t] =
  t match
    case FAux[a] => a

val a: F[{type A = Int}] = 10
val b: F[{type A = String}] = "asd"