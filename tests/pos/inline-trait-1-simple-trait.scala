inline trait A:
  type X = String

  val x: Int = 3
  val y: Int = x + 4
  // private val z: Int = 1

  def f: Int = 5
  def f(x: Int): Int = x
  def f[T](x: T): Int = 2

  private[A] def g = 1
  protected def p = 123
  // private[this] def pp = 123456

  def xx: X = "foo".asInstanceOf[X]
end A

class B extends A
