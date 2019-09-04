object Test1 {
  class A {
    override def toString: String = "A"
  }
  class B(a: A) {
    export a._  // OK
  }
}

object Test extends App {
  trait T {
    def foo: Int = 1
    def bar: Int
  }
  class A extends T {
    override def foo = 2
    override def bar = 2
  }
  class B(a: A) extends T {
    export a._
  }
  val b = B(A())
  assert(b.foo == 1)
  assert(b.bar == 2)
}