// Derives from run/i17021.defs
// but with a Java protected member
// which leads to a compile error
package p2:
  trait B extends p1.A:
    def bar: Int = foo // error: method bar accesses protected method foo inside a concrete trait method: use super.foo instead

  class C extends B:
    override def foo: Int = 2

object Test:
  def main(args: Array[String]): Unit =
    val n = new p2.C().bar
    assert(n == 2, n)
