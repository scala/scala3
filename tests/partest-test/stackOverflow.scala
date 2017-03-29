object Test {
  def foo: Int = bar
  def bar: Int = foo

  def main(args: Array[String]): Unit =
    println(foo)
}
