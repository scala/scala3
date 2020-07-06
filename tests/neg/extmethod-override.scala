class A {
  def f(x: Int)(y: Int): Int = 0
  extension (x: Int) def g(y: Int): Int = 1
}
class B extends A {
  extension (x: Int) override def f(y: Int): Int = 1  // error
  override def g(x: Int)(y: Int): Int = 0    // error
}