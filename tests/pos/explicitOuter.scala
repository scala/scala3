class Outer(elem: Int, val next: Outer) {

  trait InnerTrait {
    def foo = elem
  }

  class InnerClass extends next.InnerTrait {
    def bar = elem
  }

  class EmptyInnerClass {
    def foo = 1 // still needs outer because it is not private
  }

  def inner = {
    trait InnerTrait {
      def foo = elem
    }

    class InnerClass extends next.InnerTrait {
      def bar = elem
    }

    class EmptyInnerClass {
      def foo = 1      // does not need outer
    }

    val ic = new InnerClass
    println(ic.bar)
    println(ic.foo)
    val it = new InnerTrait {}
    println(it.foo)
    val ec = new EmptyInnerClass
  }

}

object Test extends App {

  val o = new Outer(1, new Outer(2, null))
  val ic = new o.InnerClass
  println(ic.bar)
  println(ic.foo)
  val it = new o.InnerTrait {}
  println(it.foo)
  val ec = new o.EmptyInnerClass
  o.inner
}
