object o {

  opaque type T = Int
  val x: T = 322

  def toT(x: Int): T = x

  object oo {
    def apply222(x: Int): T = x
  }
}

object Test {

  val y: o.T = o.x
  val x: Int = o.x   // error
  val z: Int = y     // error

  val t = o.toT(11)
  val i: Int = t     // error

  val t2: o.T = o.oo.apply222(22)
  val i2: Int = t2   // error
}