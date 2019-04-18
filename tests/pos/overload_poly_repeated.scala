class C {
  def foo(xs: Int*): Int = xs.toSeq.head
  def foo(x: Int): Int = x
  foo(2)

  def fooT[T](xs: T*): T = xs.toSeq.head
  def fooT[T](x: T): T = x
  fooT(2)

  def f[T](x: T): T = x
  def f[T](x: T, xs: T*): T = x

  f(5)
}
