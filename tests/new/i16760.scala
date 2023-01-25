object Test {
  def main(args: Array[String]): Unit = {
    def process(arg: Int)(block: => Unit): Int = arg
    val x = process(1)
    {
    }
    println(x)
    val y = process(2)
      ( ()
      )
    println(y)
  }
}