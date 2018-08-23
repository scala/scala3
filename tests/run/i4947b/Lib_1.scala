object Lib {
  transparent def track[T](f: => T): T = {
    printStack("track")
    printStack("track")
    f
  }
  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }

  def printStack(tag: String, i: Int): Unit = {
    println(s"$tag (i = $i): ${new Exception().getStackTrace().apply(1)}")
  }

  transparent def fact[T](transparent i: Int)(f: => T): Int = {
    printStack("track", i)
    printStack("track", i)
    track {
      printStack("fact")
    }
    f
    if (i == 0)
      1
    else {
      i * fact(i-1)(f)
    }
  }
}
