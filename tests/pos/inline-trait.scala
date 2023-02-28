inline trait A1:
  def f: Int = 2
  def f(x: Int): Int = 2
  def f[T](x: T): Int = 2

class B1 extends A1:
  /*
  <generated> override def f: Int = ???
  <generated> override def f(x: Int): Int = ???
  <generated> override def f[T](x: T): Int = ???
  */
end B1
