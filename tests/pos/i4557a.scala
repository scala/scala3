class A[X]
object O {
  type T[X, Y] = A[X]
  type S[X, Y] = T[X, Y]
  class B extends S[Int, Int]
}
