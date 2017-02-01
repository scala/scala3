class X1(val s: String) extends AnyVal {
  trait I2 { // error: value class may not define an inner class or trait
   val q: String
   def z = s + q
  }
}

class X2(val s: String) extends AnyVal {
  private[this] class I2(val q: String) // error: value class may not define an inner class or trait

  def y(i: Int) = {
    val i2 = new I2(i.toString)
    i2.q + s
  }
}

class X3(val s: String) extends AnyVal {
  object I3 // error: value class may not define non-parameter field
}
