trait A {
  def g: Int
}

class X(y: Cold[Y]) {
  class B extends A {
    @scala.annotation.cold
    def g = y.n                  // error // error
  }

  val b = new B
}

class Y {
  val x = new X(this)
  x.b.g

  val n = 10
}
