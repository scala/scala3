object Test {
  var flag = false

  def f(): Unit = assert(false)
  def f(x: Int): Unit = assert(false)
  def f(x: String): Unit = flag = true

  def foo(c: java.util.function.Consumer[String]) = c.accept("")

  def main(args: Array[String]) = {
    foo(f)
  }
}
