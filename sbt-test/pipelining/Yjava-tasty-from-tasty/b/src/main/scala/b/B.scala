package b

object B {
  val A_VALUE = (new a.A).VALUE

  @main def test = {
    assert(A_VALUE == "A", s"actually was $A_VALUE")
  }
}
