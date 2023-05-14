// scalajs: --skip

object Test {

  inline def fact[T](inline i: Int)(inline f: T): Int = {
    printStack(i, "track")
    printStack(i, "track")
    f
    if (i == 0)
      1
    else {
      i * fact(i-1)(f)
    }
  }

  def printStack(i: Int, tag: String): Unit = {
    println(s"$tag (i = $i): ${new Exception().getStackTrace().apply(1)}")
  }

  def main(args: Array[String]): Unit = {
    fact(0) {
      fact(2) {
        printStack(-1, "main1")
        printStack(-1, "main2")
      }
    }
  }

}
