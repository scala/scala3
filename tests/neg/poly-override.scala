class Super {
  def a[T] = {}
  def b[T](x: String) = {}
  def c[F[_ <: String]] = {}
  def c1[F[_ >: String]] = {}
  def d[F[_]] = {}

  def m(x: String) = {}
  def n[T](x: String) = {}
}
class Sub1 extends Super {
  override def a[T <: String] = {} // error
  override def b[T <: String](x: String) = {} // error
  override def c[F[_]] = {} // error
  override def c1[F[_]] = {} // error
  override def d[F[+_]] = {} // error

  override def m(x: Any) = {} // error
  override def n[T](x: Any) = {} // error
}

class Sub2 extends Super {
  override def a[T >: String] = {} // error
  override def b[T >: String](x: String) = {} // error
}
