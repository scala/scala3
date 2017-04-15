
object Test {
  lazy val x: true = { println("X"); true }

  def main(args: Array[String]): Unit = {
    lazy val y: true = { println("Y"); true }
    x
    y
  }
}