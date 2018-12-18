// Check that calling `.nn` on a null value throws a NPE.
object Test {
  def len(x: Array[String]|Null): Unit = x.nn.length
  def load(x: Array[String]|Null): Unit = x.nn(0)

  def check(x: => Any) = try { x; sys.error("failed to throw NPE!") } catch { case _: NullPointerException => }

  def main(args: Array[String]): Unit = {
    check(len(null)) 
    check(load(null))
  }
}
