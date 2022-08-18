trait A(o: Outer):
    def foo() = println(o.m)

class Outer:
  trait B extends A

  class C extends B with A(this)

  def bar(c: C) = c.foo()

  bar(new C) // error
  val m = 10
