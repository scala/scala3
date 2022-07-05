class A(val x: Int):
  class X:
    inline def foo() = A.this.foo()

  private def foo(): Int = x

class B extends A(20):
  val a = new A(10)
  val y: Y = new Y

  class Y extends a.X

class C:
  var b = new B
  assert(b.y.foo() == 10)

@main
def Test = new C()
