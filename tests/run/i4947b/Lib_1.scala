object Lib {
  transparent def track[T](f: => T): T = {
    printStack("track")
    printStack("track")
    f
  }
  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }
}
