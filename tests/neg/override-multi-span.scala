class A:
  def foo: Int = 0
def main(): Unit =
  println("Hello")
class B extends A:
  override def foo: String = "0"  // error
