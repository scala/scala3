class Super {
  def a[T <: String] = {}
  def b[T <: String](x: String) = {}
  def c[F[_]] = {}
  def d[F[+_]] = {}
}
class Sub1 extends Super {
  override def a[T <: AnyRef] = {}
  override def b[T <: AnyRef](x: String) = {}
  override def c[F[_ <: String]] = {}
  override def d[F[_]] = {}
}
class Sub2 extends Super {
  override def c[F[_ >: String]] = {}
}
