// Derives from run/i17021.defs
// but with a Java protected member
// which changes the behaviour
package p2:
  trait B extends p1.A:
    def bar: Int = foo

  class C extends B:
    override def foo: Int = 2

object Test:
  def main(args: Array[String]): Unit =
    val n = new p2.C().bar
    assert(n == 1, n) // B can only call super.foo
