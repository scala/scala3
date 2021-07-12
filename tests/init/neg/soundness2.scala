class C(c: C) {
  val d = c.c2          // error
  val c2 = new C(this)
}
