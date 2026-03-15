class I protected (x: Int) {
  protected def f = x
}

class M protected () extends I(42) {
  def t1 = new M()   // ok
  def t2 = new I(42) // error
  def this(x: Int) = { this(); new I(x) } // error
}
