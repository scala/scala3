class Foo {
  def A(i: Double) = i
  object A {
    def apply(i: Int): Int = i+1
  }
  def B(i: Any) = i
  object B {
    def apply(i: Int) = i+1
  }
  def C(i: Int) = i + 1
  object C {
    def apply(i: Double): Double = i
  }
  def foo = A(0)
  def bar = B(1)
  def baz = C(2)
}

object Test {
  val x = new Foo().foo
  val y = new Foo().bar
  val z = new Foo().baz
  val x1: Int = x
  val y1: Int = y
  val z1: Int = z
}

object Test2 {

  trait A {
    def apply(x: Any): Int =  1
  }

  object B {
    def f(x: Any): A = new A {}
    lazy val f: A = f(null)
  }
}