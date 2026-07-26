object Test:
  def main(args: Array[String]): Unit =
    val c = new Coder_1[Int, Int, Int](null) // error
    val c2 = new Coder_1$1[Int, Int, Int](null) // error

