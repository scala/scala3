object I0 {
  def i1(i2: => Int) = i2
  def i3[I4, I5](i6: I4 => I5): (I4 => I5) { def apply(i7: I4): I5 } = i6
  val i8 = i3(i1)
}
