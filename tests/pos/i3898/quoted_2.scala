object Test {
  def main(args: Array[String]): Unit = {
    def x: Int = 5
    Macro.ff(x)
  }
}
