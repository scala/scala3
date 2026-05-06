//> using options -opt-inline:**

// Regression test for an optimizer-only issue in which 'TR.bad' and 'C.caller' end up with different 'X.A' instances,
// due to inlining of 'TR.X' in 'TR.bad'

trait TR {
  enum X:
    case A
    case B(n: Int)
  def bad(x: X): Unit = x match {
    case X.A => ()
    case X.B(n) => println(n)
  }
}
class C extends TR {
  def caller(): Unit = bad(X.A)
}
object Test {
  def main(args: Array[String]): Unit =
    val c = new C()
    c.caller()
}
