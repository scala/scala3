trait Two[A, B]

opaque type U[A] = [B] =>> Two[A, B] // error: opaque type alias cannot have multiple type parameter lists // error: cannot instantiate
opaque type T = [A] =>> [B] =>> String // error: opaque type alias cannot have multiple type parameter lists
opaque type S = [B] =>> String // ok
opaque type IArray[+T] = Array[? <: T] // ok
opaque type S2[B] = String // ok

opaque type BadF[T] = [U] =>> (T, U)    // error
opaque type BadG = [T] =>> [U] =>> (T, U) // error


