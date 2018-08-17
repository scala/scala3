object Test {
  def main(args: Array[String]): Unit = {
    Lib.track {
      Lib.printStack("main1")
      Lib.printStack("main2")
    }
  }

}
