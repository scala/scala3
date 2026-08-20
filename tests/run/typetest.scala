object Test:
  def main(args: Array[String]): Unit =
    val i: Int = 1
    println(i.isInstanceOf[Object])
    println(1.isInstanceOf[Object])

