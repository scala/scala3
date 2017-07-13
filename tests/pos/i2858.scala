class Cont[A0](x0: A0) { type A = A0; val x: A = x0 }

object Test {
  def test: Unit = {
    val c: Cont[_] & { type A = Int } = new Cont(1)
    c.x
  }
}
