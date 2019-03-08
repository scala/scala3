object Test {
  def foo: Unit = {
    bar
  }

  def main(args: Array[String]): Unit = {
    foo
  }
}
object bar { println("Hello") }
