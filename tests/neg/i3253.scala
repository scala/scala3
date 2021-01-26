import Test.test

class A:
  def test = "  " * 10 // error
object Test extends A
