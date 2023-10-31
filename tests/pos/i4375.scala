object Test {
  type Id[A] >: A
  def test: Unit = {
    val a: Array[? >: Id[Int]] = Array(1, 2)
    val b = a(0)
  }

  class VC(i: String) extends AnyVal
  def test2: Unit = {
    val c: Array[? >: Id[VC]] = Array(new VC(""))
    val d = c(0)
  }
}
