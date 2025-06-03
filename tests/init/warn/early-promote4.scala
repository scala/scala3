abstract class A {
  bar()
  def bar(): Unit
}

class Outer {
  val a: Int = 5
  trait B {
    def bar() = assert(a == 5)
  }

  class M extends A with B {
    val n: Int = 10
  }
}

class Dummy {
  val m: Int = n + 4
  val n: Int = 10     // warn
}
