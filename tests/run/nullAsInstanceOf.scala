object Test {
  def main(args: Array[String]): Unit = {
    val u = null.asInstanceOf[Unit]
    val b = null.asInstanceOf[Byte]
    val c = null.asInstanceOf[Char]
    val s = null.asInstanceOf[Short]
    val i = null.asInstanceOf[Int]
    val l = null.asInstanceOf[Long]
    val f = null.asInstanceOf[Float]
    val d = null.asInstanceOf[Double]
    val str = null.asInstanceOf[String]

    assertIs((), u)
    assertIs(0.toByte, b)
    assertIs('\u0000', c)
    assertIs(0.toShort, s)
    assertIs(0, i)
    assertIs(0L, l)
    assertIs(0.0f, f)
    assertIs(0.0, d)
    assertIs(null, str)
  }

  def assertIs(expected: Any, actual: Any): Unit =
    assert(java.util.Objects.equals(expected, actual))
}
