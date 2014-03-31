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
  println(x.f)
  println(x.g(2))
  println(y.f)
  println(y.g(1.0))


}
