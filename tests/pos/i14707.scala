object M {
  class A {{
    def f(v: => Int): Int = 0
    def g: Int = 0
    new { f(g) }
  }}
}

object M2 {
  abstract class A {
    def local = {
      def f(v: () => Int): Int = 0
      def g(): Int = 0
      new AnyRef { def h = f(() => g()) }
    }
  }
}