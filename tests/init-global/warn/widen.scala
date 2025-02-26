trait T {
  def foo(): Unit
}

class C extends T {
  def foo(): Unit = println("Calling foo on an instance of C!")
}

object O:
  def bar(t: T) = {
    class A {
      class B {
        t.foo() // warn
      }

      val b = new B
    }
    new A
  }
  val a = bar(new C)