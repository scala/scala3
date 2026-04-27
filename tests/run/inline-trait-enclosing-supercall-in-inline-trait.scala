trait A:
  def foo = 10

class B extends A:
  inline trait T:
    def foo = B.super.foo

  class C2 extends T

@main def Test = 
  val x = new B()
  val y = new x.C2()
  assert(y.foo == 10)
