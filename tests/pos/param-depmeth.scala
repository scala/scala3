object Test {

  class C { type T }

  def f(x: C, y: x.T): x.T = y // ok

  val c = new C { type T = String }
  val c2 = c

  f(c, "abc")
  f(new C{ type T = String}, "abc")

  val d: (C{ type T = String}) # T = "abc"

}
