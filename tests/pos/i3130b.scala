trait F { def f(): Int }
class Outer {
  inline def inner: F = {
    class InnerClass(x: Int) extends F {
      def this() = this(3)
      def f() = x
    }
    new InnerClass(3)
  }
}

object Test extends App {
  val o = new Outer
  assert(o.inner.f() == 3)
}
