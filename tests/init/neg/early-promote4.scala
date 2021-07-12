abstract class A {
  bar()
  def bar(): Unit
}

class Outer {
  val a: Int = 5
  trait B {
    def bar() = assert(a == 5)
  }
}

class M(val o: Outer) extends A with o.B {
  val n: Int = 10
}

class Dummy {
  val m: Int = n + 4
  val n: Int = 10     // error
}