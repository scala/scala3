abstract class A {
  def f: () => Int
  val m = f

  @scala.annotation.init
  def g: Int
}

class C {
  class B extends A {
    def f = () => x           // error
    def g = 10
  }

  new B

  val x = 10
}

class D {
  class B extends A {
    def f = () => 5
    def g = x                 // error  // error
  }

  class C extends B {
    g                         // error
  }

  new C                       // error

  val x = 10
}