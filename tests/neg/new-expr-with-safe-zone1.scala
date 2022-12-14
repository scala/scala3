import language.experimental.captureChecking

object NegTest {
  class A(x: Int) {}

  def test(): Unit = {
    val str = "abc"
    val a1 = new {str} A(0) // error // nopos-error // nopos-error
  }
}