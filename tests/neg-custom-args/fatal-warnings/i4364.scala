object Test {
  def foo(c: java.util.function.Consumer[Integer]) = c.accept(0)
  def f(x: Int): Unit = ()

  def main(args: Array[String]) = {
    foo(f) // Ok: Consumer is @FunctionalInterface
    new java.io.ObjectOutputStream(f) // error: OutputStream is not @FunctionalInterface
  }
}
