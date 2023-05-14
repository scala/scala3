class C(c: C) {
  val d = c.c2
  val c2 = new C(this)  // error
}
