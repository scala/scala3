object Test {

  def main(args: Array[String]): Unit = {
    println(Foo.inspectBody(foo))
    println(Foo.inspectBody(bar))

    3 match {
      case x =>
        println(Foo.inspectBody(x))
    }
  }

  def foo: Int = 1 + 2
  val bar: Int = 2 + 3
}
