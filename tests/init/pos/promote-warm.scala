class PromoteWarm:
  class A:
    def foo() = PromoteWarm.this.foo()

  class B(a: A):
    a.foo()

  val n = 10
  val a = new A
  val b = new B(a)

  def foo() = println(n)
