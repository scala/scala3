
object Test {
  def main(args: Array[String]): Unit = {
    println(Foo.inspectBody(foo))
    println(Foo.inspectBody(bar))
  }

  def foo: Int = 1 + 2
  val bar: Int = 2 + 3
}
