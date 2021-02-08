package test

package p {
  class A(a: String = "")
}

package object po {
  type A = p.A
}

import po.*
class C {
  val a = new A() //p.A.init$default$1)
}
