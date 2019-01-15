object Test {
  def foo: Unit = {
    object bar { println("Hello") }
    bar
  }

  def main(args: Array[String]): Unit = {
    foo
  }
}
