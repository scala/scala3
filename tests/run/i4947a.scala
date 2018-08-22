object Test {

  transparent def fact[T](transparent i: Int)(f: => T): Int = {
    printStack("track", i)
    printStack("track", i)
    f
    if (i == 0)
      1
    else {
      i * fact(i-1)(f)
    }
  }

  def printStack(tag: String, i: Int): Unit = {
    println(s"$tag (i = $i): ${new Exception().getStackTrace().apply(1)}")
  }

  def main(args: Array[String]): Unit = {
    fact(2) {
      printStack("main1", -1)
      printStack("main2", -1)
    }
  }

}
