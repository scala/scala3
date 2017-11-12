object Test {

  trait C { type M; val m: M }

  type DF = (x: C) => x.M
  val depfun: DF = ??? // (x: C) => x.m
  val c = new C { type M = Int; val m = 0 }
  val y = depfun(c)
  val y1: Int = y

  val d: C = c
  val z = depfun(d)
  val z1: d.M = z
}