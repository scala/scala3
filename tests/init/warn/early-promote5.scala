abstract class A {
  bar(this)
  def bar(x: A): Unit
}

class Outer {
  val a: Int = 4
  trait B {
    def bar(x: A) = println(a)
  }

  class M(c: Container) extends A with B
}

class Container {
  val o = new Outer
  val m = new o.M(this)   // warn
  val s = "hello"
}

class Dummy {
  val m: Int = n + 4
  val n: Int = 10     // warn
}
