package b

import a.A

class B[T] {
  val inner = new A[T](23) {}
}

object B {
  val derived: Int = (new B[Int]).inner.value
}

