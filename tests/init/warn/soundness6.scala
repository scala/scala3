class C(c: C) {
  println(c.n)
  val c2 = new C(this)  // warn
  val n = 10
}
