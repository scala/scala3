trait T {
  object O

  type U = (given Int) => Int
  type V = (given Int) => Int

  val u = delegate (x: Int) => x
  val v = given (x: Int) => x
  val w = (given x: Int) => x
  val w2: V = (given x) => x
}
