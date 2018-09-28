class I0 {
  class I1
  def i3 = {
    type T = i5.I1
    val i4 = new T // error: `i5` is a forward reference extending over the definition of `i4`
    val i5 = new I0
  }
}
