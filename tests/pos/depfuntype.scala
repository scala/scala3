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
}
