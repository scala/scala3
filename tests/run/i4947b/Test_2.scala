// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    Lib.track {
      Lib.printStack("main1")
      Lib.printStack("main2")
    }
    Lib.track {
      Lib.track {
        Lib.printStack("main3")
        Lib.printStack("main4")
      }
    }
    Lib.fact(0) {
      Lib.fact(2) {
        Lib.printStack("main1", -1)
        Lib.printStack("main2", -1)
      }
    }
  }

}
