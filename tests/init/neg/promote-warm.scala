class PromoteWarm:
  class A:
    def foo() = PromoteWarm.this.foo()

  class B(a: A):
    a.foo()

  val a = new A
  val b = new B(a)  // error
  val n = 10

  def foo() = println(n)
