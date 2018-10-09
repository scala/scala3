object Foo {

  inline def track[T](f: => T): T = {
    printStack("track")
    printStack("track")
    f
  }

  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }
}

object Test {
  import Foo._
  def main(args: Array[String]): Unit = {
    track {
      printStack("main1")
      printStack("main2")
    }
  }

}
