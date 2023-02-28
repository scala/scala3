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

inline trait A2[T]:
  def f: T = f
  def f(x: T): T = x
  def f[U <: T](x: U, y: T): T = x

class B2 extends A2[Int]:
  /*
  <generated> override def f: Int = ???
  <generated> override def f(x: Int): Int = ???
  <generated> override def f[U <: Int](x: U, y: Int): Int = ???
  */
end B2