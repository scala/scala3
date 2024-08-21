//> using options -experimental

import scala.annotation.publicInBinary

package p {
  class A {
    @publicInBinary protected def foo(): Int = 1
    @publicInBinary protected def fuzz(): Int = 2
  }
}
package q {
  class B extends p.A {  // protected accessor for foo
    trait BInner {
      def bar() = foo()
    }
  }
  trait Inner extends p.A {
    def bar() = foo()   // shared super accessor for foo
                        // new super accessor for fuzz
    class InnerInner {
      def bar() = foo()
      def baz() = fuzz()
    }
  }
}
object Test extends App {
  val b = new q.B
  val bi = new b.BInner {}
  assert(bi.bar() == 1)

  class C extends p.A with q.Inner {
    // implements super accessors for foo and fuzz
  }
  val c = new C
  assert(c.bar() == 1)

  val d = new c.InnerInner
  assert(d.bar() == 1)
  assert(d.baz() == 2)
}
