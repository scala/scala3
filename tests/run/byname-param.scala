object Test {

  // Devalify shouldn't optimize this
  def theTrap(cond: Boolean, t: => Unit) = {
    val a,b = t
    if (cond) println(a) else println(b)
  }

  def main(args: Array[String]): Unit = {
    theTrap(true, println(1))
  }
}
