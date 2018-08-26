object i0 {
  // Adding anything in front of asInstanceOf,
  // i0 or this, makes the error go away.
  def i1: Int = asInstanceOf[Int].toInt

  val i2 = asInstanceOf[Int]
}
