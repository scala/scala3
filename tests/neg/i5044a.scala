class I0 {
  class I1
  def i3 = {
    val i4 = new i5.I1 // error: `i5` is a forward reference extending over the definition of `i4`
    val i5 = new I0
  }
}
