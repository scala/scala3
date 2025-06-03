trait A:
  type T
  type U = T

trait B extends A:
  type T = String

object C extends B


type F[X] = A { type U = X } // works when `U` is replaced with `T`

type InvF[Y] = Y match
  case F[x] => x


object Test:
  summon[InvF[C.type] =:= String] // ok
  summon[InvF[B] =:= String] // error: selector B does not uniquely determine parameter x
