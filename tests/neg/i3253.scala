import Test.test

class A with
  def test = "  " * 10 // error
object Test extends A
