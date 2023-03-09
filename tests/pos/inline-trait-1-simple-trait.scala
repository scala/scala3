inline trait A:
  type X = String

  val x: Int = 3
  val y: Int = x + 1

  def f: Int = f
  def f(x: Int): Int = x
  def f[T](x: T): Int = 2

  def xx: X = "foo".asInstanceOf[X]
end A

class B extends A:
  /*
  <generated> override type X = String

  <generated> override val x: Int = 3
  <generated> override val y: Int = x + 1

  <generated> override def f: Int = 2
  <generated> override def f(x: Int): Int = 2
  <generated> override def f[T](x: T): Int = 2

  <generated> override def xx: X = "foo".asInstanceOf[X]
  */
end B
