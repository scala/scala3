package p1:
  class A:
    protected val foo: Int = 1

package p2:
  trait B extends p1.A:
    def bar: Int = foo

  class C extends B: // was: error: parent trait B has a super call which binds to the value p1.A.foo. Super calls can only target methods.
    override val foo: Int = 2

// Also, assert that the access continues to delegate:
// i.e. B#bar delegates to this.foo and so C#bar returns 2,
// not B#bar delegates to super.foo and so C#bar returns 1.
object Test:
  def main(args: Array[String]): Unit =
    val n = new p2.C().bar
    assert(n == 2, n) // was: assertion failed: 1
