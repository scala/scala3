class Outer(elem: Int, val next: Outer) {

  trait InnerTrait {
    def foo = elem
  }

  class InnerClass(x: Int) extends next.InnerTrait {
    def this() = this(3)
    def bar = elem + x
  }

  class EmptyInnerClass {
    def foo = 1 // still needs outer because it is not private
  }

  def inner = {
    trait InnerTrait {
      def foo = elem
    }

    class InnerClass(x: Int) extends next.InnerTrait {
      def this() = this(3)
      def bar = elem + x
    }

    class EmptyInnerClass {
      def foo = 1      // does not need outer
    }

    val ic = new InnerClass(1)
    println(ic.bar)
    println(ic.foo)
    val it = new InnerTrait {}
    println(it.foo)
    val ec = new EmptyInnerClass
  }

  def inner2 = {
    class C {
      val x = elem
    }
    class D {
      new C
    }
    class E {
      f()
    }
    def f() = ()
  }
}

object Test extends App {

  val o = new Outer(1, new Outer(2, null))
  val ic = new o.InnerClass(1)
  println(ic.bar)
  println(ic.foo)
  val it = new o.InnerTrait {}
  println(it.foo)
  val ec = new o.EmptyInnerClass
  o.inner
}
