// Test the `nn` extension method for removing nullability.
object Test {
  def len(x: Array[String]|Null): Unit = x.nn.length
  def load(x: Array[String]|Null): Unit = x.nn(0)

  def assertThrowsNPE(x: => Any) = try {
    x;
    assert(false) // failed to throw NPE
  } catch { case _: NullPointerException => }

  def main(args: Array[String]): Unit = {
    assert(42.nn == 42)
    val x: String|Null = "hello"
    assert(x.nn == "hello")
    val y: String|Null = null
    assertThrowsNPE(y.nn)
    assertThrowsNPE(null.nn)
    assertThrowsNPE(len(null))
    assertThrowsNPE(load(null))
  }
}
