object Test {
  def foo(c: java.util.function.Consumer[String]) = c.accept("")

  def f(x: String): Unit = ()
  def f(x: Int): Unit = ()

  def main(args: Array[String]) = {
    foo(f) // Ok: Consumer is @FunctionalInterface

    val oos = new java.io.ObjectOutputStream(f) // error: OutputStream is not @FunctionalInterface
    oos.write(0)
    oos.close()
  }
}
