object Test {
  def main(args: Array[String]): Unit = {
    printlnStackLine(f1)
    printlnStackLine(f2)
    printlnStackLine(f3)
    printlnStackLine(f4)
    printlnStackLine(f5)
    printlnStackLine(f6)
  }

  def f1: Unit = {
    val a: Nothing =
      null.asInstanceOf[Nothing] // throws here
  }

  def f2: Unit = {
    null.asInstanceOf[Nothing] // throws here
  }

  def f3: Unit = {
    null.asInstanceOf[Nothing] // throws here
    ()
  }


  def f4: Unit = {
    val n: Any = null
    n.asInstanceOf[Nothing] // throws here
    ()
  }

  def f5: Unit = {
    val n: Any = null
    val a: Nothing =
      n.asInstanceOf[Nothing] // throws here
    ()
  }

  def f6: Unit = {
    val n: Any = null
    val a: Nothing =
      { println("foo"); n }.asInstanceOf[Nothing] // throws here
    ()
  }

  def printlnStackLine(t: => Any): Unit = {
    try t
    catch {
      case e: ClassCastException =>
        println(e.getStackTrace.head)
    }
  }
}
