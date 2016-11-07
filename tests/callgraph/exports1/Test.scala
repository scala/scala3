
object Test {
  def main(args: Array[String]): Unit = {
  }

  // TODO check that it is not DCEed
  @scala.export def foo() = 42
}
