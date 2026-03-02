package b

import a.A

object B {
  val finalResult: "A" = A.VALUE

  val a_B: String = (new A()).add("B")
  val a_true: String = (new A()).add(true)

  @main def test = {
    assert(finalResult == "A", s"actually was $finalResult")
    assert(a_B == "AB", s"actually was $a_B")
    assert(a_true == "Atrue", s"actually was $a_true")
  }
}

