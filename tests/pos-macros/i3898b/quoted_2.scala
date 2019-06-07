object Test {
  def main(args: Array[String]): Unit = {
    def z: Int = 5
    Macro.ff(z, 5)
  }
}
