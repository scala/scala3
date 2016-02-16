class A

object Impl {
  def foo()(implicit x: A = null): Int = 2
  def test: Int = {
    foo()() // ok
    foo()   // did not work before, does now
  }
}

// same with multiple parameters
object Impl2 {
  def foo()(implicit ev: Int, x: A = null): Int = 2
  def test: Int = {
    implicit val ii: Int = 1
    foo()
  }
}
