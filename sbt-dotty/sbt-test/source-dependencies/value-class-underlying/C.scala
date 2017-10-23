object C {
  def main(args: Array[String]): Unit = {
    val x = B.foo
    println("x: " + x) // Need to use x in an expression to see if it crashes or not
  }
}
