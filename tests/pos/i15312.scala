type F[t] =
  t match
    case {type A = Float} => Int
    case {type A = Int} => String

val a: F[{type A = Float}] = 10
val b: F[{type A = Int}] = "asd" // Found:("asd" : String) Required: F[Object{A = Int}]