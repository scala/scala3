inline trait A:
  def x: Int

class B extends A:
  override def x: Int = 1
  def f = x