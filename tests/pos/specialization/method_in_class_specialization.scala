object method_in_class_specialization {
  class A {
    def foo[@specialized(Int, Long) T](a: T) = List()
  }
  class B[K] {
    def foo[@specialized T](b: T) = List()
  }
  class C extends A {
    override def foo[@specialized T](c: T) = super.foo(c)
    def bar[@specialized(Float, Char) U](c: U) = super.foo(c)
    def man[@specialized V](c: V) = List()
  }
  class D extends B {
    override def foo[@specialized T](d: T) = super.foo(d)
    def bar[@specialized U](d: U) = super.foo(d)
    def man[@specialized V](d: V) = List()
  }
  class E[U] extends B {
    override def foo[@specialized T](e: T) = super.foo(e)
    def bar[@specialized U](e: U) = super.foo(e)
    def man[@specialized V](e: V) = List()
  }

  val a = new A
  val b = new B[Int]
  val c = new C
  val d = new D
  val e = new E[Char]

  a.foo(1)
  a.foo(1.toLong)
  a.foo("foo")
  b.foo(2)
  c.foo(3)
  d.foo(4)
  e.foo(5)

  c.bar('d')
  c.bar(6)
  e.bar('d')
  e.bar(7)
}