object Test {
  def main(args: Array[String]): Unit = {
    val a = { 5 }.asInstanceOf[Unit]
    val b = {
      println("hello!")
      5
    }.asInstanceOf[Unit]
  }
}
