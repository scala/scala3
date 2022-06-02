abstract class A {
  bar(this)
  def bar(x: A): Unit
}

class Outer {
  val a: Int = 4
  trait B {
    def bar(x: A) = println(a)
  }
}

class M(val o: Outer, c: Container) extends A with o.B

class Container {
  val o = new Outer
  val m = new M(o, this)   // error
  val s = "hello"
}

class Dummy {
  val m: Int = n + 4
  val n: Int = 10     // error
}
