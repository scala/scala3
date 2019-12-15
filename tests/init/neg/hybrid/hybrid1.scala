trait A {
  def g: Int
}

class X(_y: Y) {
  class B extends A {
    def g = _y.n
  }
}

class Y {
  val x = new X(this)

  class C extends x.B {
    g
  }

  new C

  val n = 10   // error
}
