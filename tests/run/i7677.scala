object Test {
  def main(args: Array[String]): Unit = {
    val a: Double = Double.NaN
    val eval = (a <= 0) || (10L <= 0)
    assert(!eval)
    val eval2 = (Double.NaN <= 0) || (10L <= 0)
    assert(!eval2)

    val b: Float = Float.NaN
    val eval3 = (b <= 0) || (10L <= 0)
    assert(!eval3)
    val eval4 = (Float.NaN <= 0) || (10L <= 0)
    assert(!eval4)
  }
}
