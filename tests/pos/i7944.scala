package hello

object HelloWorld {
  def main(args: Array[String]): Unit =
    println(erased(5))

  def erased(x: Int): Any = x
}
