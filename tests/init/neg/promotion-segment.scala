class Outer:
  trait A:
    def foo() = println(m)

  trait B extends A

  class C extends B

  def bar(c: C) = c.foo()

  bar(new C)  // error
  val m = 10
