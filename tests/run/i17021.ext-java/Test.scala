// scalajs: --skip
// Derives from run/i17021.defs
// but with a Java protected member
// and fixed calling code, that uses super
package p2:
  trait B extends p1.A:
    def bar: Int = super.foo

  class C extends B:
    override def foo: Int = 2

object Test:
  def main(args: Array[String]): Unit =
    val n = new p2.C().bar
    assert(n == 1, n)
