object unions {

  class A {
    def f: String = "abc"

    def g(x: Int): Int = x
    def g(x: Double): Double = x
  }

  class B {
    def f: String = "bcd"

    def g(x: Int) = -x
    def g(x: Double): Double = -x
  }

  val x: A | B = if (true) new A else new B
  def y: B | A = if (true) new A else new B
  println(x.f)  // error
  println(x.g(2)) // error
  println(y.f) // error
  println(y.g(1.0)) // error

  class C {
    private def foo = 0
    class D extends C {
      private def foo = 1
      def test(cd: C | D, dc: D | C) = (cd.foo, dc.foo)
    }
  }

}
