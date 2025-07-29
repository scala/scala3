abstract class T {
  def foo() = {
    def bar() = 5
    bar()
  }
}

class A extends T {}
class B extends T {}
class C extends T {}

object O {
  val a = new A
  val b = new B
  val c = new C
  val d = a.foo()
  val e = b.foo()
  val f = c.foo()
}