class C(_c: C) {
  println(_c.n)
  val c2 = new C(this)
  val n = 10            // error
}
