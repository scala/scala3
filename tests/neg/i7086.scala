object Test1 {
  class A {
    override def toString: String = "A"
  }
  class B(a: A) {
    export a.toString  // error: no eligible member toString at B.this.a
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
    export a.foo // error: no eligible member foo at B.this.a
    export a.bar // OK
  }
}