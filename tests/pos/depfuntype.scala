object Test {

  trait C { type M; val m: M }

  type DF = (x: C) => x.M
  val depfun1: DF = (x: C) => x.m
  val c = new C { type M = Int; val m = 0 }
  val y = depfun1(c)
  val y1: Int = y

  def depmeth(x: C) = x.m
  val depfun2 = depmeth
  val depfun3: DF = depfun2

  val d: C = c
  val z = depfun3(d)
  val z1: d.M = z

  // Reproduced here because the one from DottyPredef is lacking a Tasty tree and
  // therefore can't be inlined when testing non-bootstrapped.
  // But inlining `implicitly` is vital to make the definition of `ifun` below work.
  inline final def implicitly[T](implicit ev: T): T = ev

  type IDF = implicit (x: C) => x.M

  implicit val ic: C = ???

  val ifun: IDF = implicitly[C].m

  val u = ifun(c)
  val u1: Int = u

  val v = ifun(d)
  val v1: d.M = v
}
