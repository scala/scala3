object Test {
  def main(args: Array[String]): Unit = {
    System.out.println((() => 42)())
  }
}