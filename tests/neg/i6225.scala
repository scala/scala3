object O1 { // error: cannot be instantiated
  type A[X] = X
  opaque type T = A // error: opaque type alias must be fully applied
}

object O2 {
  opaque type A[X] = X
  object A {
    opaque type T = A  // error: opaque type alias must be fully applied
  }
}

object O3 {
  opaque type R[X] = R[X]   // error: R does not take parameters // error: cyclic
}

object O4{
  opaque type T[X] = Any
}

object O5{
  opaque type T[X] = Nothing
}
