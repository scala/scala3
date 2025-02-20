object Test:
  def main(args: Array[String]): Unit =
    foo(3)()

  def foo(x: Int)(
    y: Int = x + 1
  ): Unit =
    println("foo")
