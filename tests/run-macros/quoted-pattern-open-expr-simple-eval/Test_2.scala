
object Test {
  def main(args: Array[String]): Unit = {
    println(eval {
      val x: Int = 4
      x * x
    })
  }
}
