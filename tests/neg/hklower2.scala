class Test { // error: conflicting bounds
  trait T[X]
  type Z[X] >: String <: T[X]
}
