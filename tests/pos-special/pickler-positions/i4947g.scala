object Test {

  inline def fact[T](inline i: Int)(f: => T): Int = {
    printStack("track", i)
    printStack("track", i)
    f
    if (i == 0)
      1
    else {
      i * fact(i-1)(f)
    }
  }

  def printStack(tag: String, i: Int): Unit = ()

  fact(0) {
    fact(2) {
      printStack("main1", -1)
      printStack("main2", -1)
    }
  }

}
