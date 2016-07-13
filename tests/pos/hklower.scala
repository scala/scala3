class Test { // error: conflicting bounds

  type T[X]
  type U[X] = T[X]

  type V[X] >: T[X]
  type W[X] >: T[X] <: T[X]

  def f[C[X] >: T[X]]() = ???

}
