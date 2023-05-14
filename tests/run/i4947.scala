// scalajs: --skip

object Test {

  inline def track[T](inline f: T): T = {
    printStack("track")
    printStack("track")
    f
  }

  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }

  def main(args: Array[String]): Unit = {
    track {
      printStack("main1")
      printStack("main2")
    }
  }

}
