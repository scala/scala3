
class P[T]
//class Q[T] extends P[R[T]] // ok
class Q[T <: P[Any]] extends P[R[T]] // error
//type Q[T <: P[Any]] <: P[R[T]] // ok

type R[U] = U match
  case Q[t] => R[t]
  case P[t] => t
