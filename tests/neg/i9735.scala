trait Two[A, B]

opaque type U[A] = [B] =>> Two[A, B] // error: opaque type alias must be fully applied // error: cannot instantiate
opaque type T[A] = [B] =>> String // error: opaque type alias must be fully applied
opaque type S = [B] =>> String // error: opaque type alias must be fully applied

