abstract class A {
  def f: () => Int
  val m = f

  def g: Int
}

class C {
  class B extends A {
    def f = () => x
    def g = 10
  }

  new B

  val x = 10
}

class D {
  class B extends A {
    def f = () => 5
    def g = x
  }

  class C extends B {
    g
  }

  new C

  val x = 10   // warn
}