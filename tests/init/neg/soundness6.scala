class C(c: C) {
  println(c.n)
  val c2 = new C(this)  // error
  val n = 10
}
