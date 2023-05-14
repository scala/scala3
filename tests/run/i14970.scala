object Test {
  def main(args: Array[String]): Unit = {
    val x: Array[Int | Nothing] = Array()
    val y: Array[Int] = x
  }
}
