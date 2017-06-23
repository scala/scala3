// to be compiled with -strict
object trailingUnderscore {

  def f(x: Int) = x
  def g() = f(2)
  def h = g()

  val x1 = f _ // error
  val x2 = g _ // error
  val x3 = h _ // error
}