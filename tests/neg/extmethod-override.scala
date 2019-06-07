class A {
  def f(x: Int)(y: Int): Int = 0
  def (x: Int) g (y: Int): Int = 1
}
class B extends A {
  override def (x: Int) f (y: Int): Int = 1  // error
  override def g(x: Int)(y: Int): Int = 0    // error
}