trait I0 {
  type I1[_, _]
  type I2[_]
  type I3[_]
  trait I4 { type I5 = I1[I2[Int], I3[Int]] }
  val I6: I4
  val I7 = new I0 {
    type I1[i8] = Int
    val I6 = new { def i9[I1[_]](i10: I1[Int]): I1[I5] = i10 }
  }
}
