object Test {
  def main(args: Array[String]): Unit = {
    assert(classOf[Test.type] == Test.getClass)
  }
}
