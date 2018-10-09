
object Test {
  def main(args: Array[String]): Unit = {
    println(Foo.inspectBody(Foo.foo))
    println(Foo.inspectBody(Foo.bar))
  }
}
