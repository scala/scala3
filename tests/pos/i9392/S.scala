class S1 extends pkg.J {
  override def i(): Int = 2
}

// Unlike Scala 2 this doesn't compile, because this override of `i()`
// also matches the non-overridable field `i`.
// class S2 extends pkg.J {
//   override def i: Int = 2
// }

object Test {
  val s1 = new S1

  val i1 = s1.i
  val i2 = s1.i()

  // val s2 = new S2

  // val i3 = s2.i
}
