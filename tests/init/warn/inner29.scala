class A(x: Int) {
  def f: Int = 10
  class Inner {
    def g: Int = f
  }
}

abstract class B(n: Int) {
  val a: A
  val inner = new a.Inner
}

class C extends B(5) {
  class E extends A(10) {
    override def f: Int = x
  }

  val a = new E        // warn: init too late

  val x = 10
}