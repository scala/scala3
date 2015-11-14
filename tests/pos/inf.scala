object Test {

  def f[T](x: T, y: T): T = x
  def g[T](x: T)(y: T): T = x

  val x: Int = 1
  val y: Long = x

  val xs: Seq[Int] = Seq(1)
  val ys: Traversable[Int] = xs

  val r1 = f(x, y)
  val s1: AnyVal = r1
  val r2 = f(y, x)
  val s2: AnyVal = r2
  val r3 = f(xs, ys)
  val s3: Traversable[Int] = r3
  val r4 = f(ys, xs)
  val s4: Traversable[Int] = r4
  val r5 = g(x)(y)
  val s5: AnyVal = r5
  val r6 = g(y)(x)
  val s6: AnyVal = r6
  val r7 = g(xs)(ys)
  val s7: Traversable[Int]= r7
  val r8 = g(ys)(xs)
  val s8: Traversable[Int] = r8
}
