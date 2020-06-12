object Test {
  opaque type A[T] = T match { case Int => Int } // error: Modifier `opaque` is not allowed for this definition
}
