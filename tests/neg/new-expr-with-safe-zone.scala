import language.experimental.captureChecking

// Report error when not in scala native environment.
object NegTest {
  class A(x: Int) {}

  def test(): Unit = {
    val str = "abc"
    val a1 = new {str} A(0) // error // error
  }
}