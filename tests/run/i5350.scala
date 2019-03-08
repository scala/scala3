object Test {
  def foo = {
    lazy val bar: Unit = println("Hello")
    bar
  }

  def main(args: Array[String]): Unit = {
    foo
  }
}
