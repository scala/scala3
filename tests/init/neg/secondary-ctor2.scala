class A(b: B, x: Int) {
  def this(b: B) = {
    this(b, 5)
    class Inner() {
      def foo() = println(b.n)
    }
    Inner().foo() // error: calling method on cold

    val f = () => new A(b, 3)
    f() // ok
  }
}

class B(val d: D) {
  val n: Int = 10
}

class C(b: B) extends A(b) {
  def this(b: B, x: Int) = this(b)
}

class D {
  val b = new B(this)
  val c = new C(b, 5)
}
