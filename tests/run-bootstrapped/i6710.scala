object Test {
  def main(args: Array[String]): Unit = {
    // constant fold will fail for non-bootstrapped Dotty
    assert(!(Float.NaN > 0.0))
    assert(!(Float.NaN < 0.0))
    assert(!(Double.NaN > 0.0))
    assert(!(Double.NaN < 0.0))

    val f: Float = Float.NaN
    val d: Double = Double.NaN
    assert(!(f > 0.0f))
    assert(!(f < 0.0f))
    assert(!(d > 0.0))
    assert(!(d < 0.0))

    // loop forever before the fix
    var x = Double.NaN
    while(x < 10.0) { x = x + 1; println(x) }
    while(x > 10.0) { x = x + 1; println(x) }
    do { x = x + 1; println(x) } while(x < 10.0)
    do { x = x + 1; println(x) } while(x > 10.0)
  }
}
