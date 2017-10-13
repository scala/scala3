object Test {

  def main(args: Array[String]): Unit = {
    {
      unused (x: Int) => 42
    }

    println("ok")
  }
}
