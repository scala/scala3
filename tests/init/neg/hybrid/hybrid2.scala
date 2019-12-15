trait A {
  def g: Int
}

class X(_y: Y) {
  class B extends A {
    def g = _y.n                  // error
  }

  val b = new B
}

class Y {
  val x = new X(this)
  x.b.g

  val n = 10
}
