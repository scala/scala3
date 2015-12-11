class Test {

  type T[X] // OK
  type U[X] = T[X] // OK

  type V[X] >: T[X] // error
  type W[X] >: T[X] <: T[X] // error

  def f[C[X] >: T[X]]() = ??? // error

}
