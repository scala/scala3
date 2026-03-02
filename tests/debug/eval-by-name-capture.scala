object Test:
  def main(args: Array[String]): Unit =
    val x = "hello"
    m:
      x + ", world!"

  def m(y: => String): Unit = println(y)
