trait A {
  @scala.annotation.init
  def g: Int
}

class X(y: Cold[Y]) {
  class B extends A {
    def g = y.n
  }
}

class Y {
  val x = new X(this)

  class C extends x.B {       // error
    g
  }

  new C                       // error

  val n = 10
}


class Z(y: Cold[Y]) {
  @scala.annotation.init
  class B extends A {
    def g = y.n            // error
  }
}