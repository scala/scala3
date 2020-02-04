object O1 {
  type A[X] = X
  opaque type T = A
}

object O2 {
  opaque type A[X] = X
  object A {
    opaque type T = A
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
