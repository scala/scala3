class C(c: C) {
  println(c.n)          // error
  val c2 = new C(this)
  val n = 10
}
