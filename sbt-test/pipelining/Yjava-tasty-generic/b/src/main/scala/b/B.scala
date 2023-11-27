package b

import a.A

class B[T] {
  val inner = new A[T](23) {}
}

object B {
  @main def test = {
    val derived: Int = (new B[Int]).inner.value
    assert(derived == 23, s"actually was $derived")
  }
}

