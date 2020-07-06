object Test {
  def main(args: Array[String]): Unit = {
    assert(isHello(extension_strip("hello")))
    assert(!isHello(extension_strip("bonjour")))
  }
}