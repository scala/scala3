package b

import a.A

object B {
  val finalResult: "A" = A.VALUE

  val a_B: String = (new A()).add("B")
  val a_true: String = (new A()).add(true)

  @main def test = {
    assert(finalResult == "A")
    assert(a_B == "AB")
    assert(a_true == "Atrue")
  }
}

