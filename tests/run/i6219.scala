object Test {
  def main(args: Array[String]): Unit = {
    assert(s"hello ${} world" == "hello () world")
    assert(s"hello ${  } world" == "hello () world")
  }
}
