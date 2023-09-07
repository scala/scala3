// Derives from run/i17021, but with defs instead of vals
package p1:
  class A:
    protected def foo: Int = 1

package p2:
  trait B extends p1.A:
    def bar: Int = foo

  class C extends B:
    override def foo: Int = 2

object Test:
  def main(args: Array[String]): Unit =
    val n = new p2.C().bar
    assert(n == 2, n) // was: assertion failed: 1
