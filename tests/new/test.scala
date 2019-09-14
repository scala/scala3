trait T {
  object O

  type U = (given Int) => Int
  type V = (given Int) => Int

  val u = delegate (x: Int) => x
  val v = given (x: Int) => x
  val w = (given x: Int) => x
  val w2: V = (given x) => x

  class C[T]

  given t2[T](given C[T]): C[T]
}
