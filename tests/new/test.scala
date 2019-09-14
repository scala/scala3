trait T {
  object O

  type U = delegate Int => Int
  type V = given Int => Int

  val u = delegate (x: Int) => x
  val v = given (x: Int) => x
}
