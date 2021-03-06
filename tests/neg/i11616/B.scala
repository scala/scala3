package pkg.sub

import pkg.A

class B extends A {

  def test(): Unit = {
    super.fn1()
    super.fn2() // error: cannot be acesssed

    val b: B = new B()
    b.fn1()
    b.fn2()     // error: cannot be acesssed
  }

}
