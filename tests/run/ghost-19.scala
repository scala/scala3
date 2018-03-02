object Test {

  def main(args: Array[String]): Unit = {
    {
      ghost (x: Int) => 42
    }

    println("ok")
  }
}
