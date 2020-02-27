object Test {
  inline def foo: String = scala.compiletime.error(s"")

  def main(args: Array[String]): Unit = println(foo) // error
}
