 object Test {
  def main(args: Array[String]): Unit = {
    println(new FInterpolatorHelper(StringContext("hello%s")).ff(5))
  }
}
