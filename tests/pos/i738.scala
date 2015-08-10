object Test {
  def twoN[T](x: T)(implicit ev: T): Nothing = ???
  def twoT[T](x: T)(implicit ev: T): T = ???

  def test = {
    implicit val i: Int = 1

    twoN(42) // dotty with #738: T=Any, implicit search fails; scalac: T=Int, implicit search succeeds
    twoT(42) // dotty with #738 and scalac: T=Int, implicit search succeeds
  }
}
