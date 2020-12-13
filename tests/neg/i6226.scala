object O1 {
  opaque type X = Array[X] // error
}

object O2 {
  opaque type X = X // error
}

object O3 { // error
  opaque type R[X] = T[X] // error
  opaque type T[X] = R[X] // error
}
