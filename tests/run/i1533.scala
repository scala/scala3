object Test {
  def main(args: Array[String]): Unit = {
    val x = (if (1 == 1) 1 else 2.0): Any
    assert(x.isInstanceOf[java.lang.Integer])
    assert(x == 1)
  }
}
