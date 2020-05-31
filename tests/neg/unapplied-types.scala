trait T {
  type L[X] = List[X]
  type T1 <: L    // error: takes type parameters
  type T2 = L     // error: takes type parameters
  type T3 = List  // error: takes type parameters
  type T4 <: List // error: takes type parameters
}
