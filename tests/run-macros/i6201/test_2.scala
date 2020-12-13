object Test {
  def main(args: Array[String]): Unit = {
    assert(isHello(strip("hello")))
    assert(!isHello(strip("bonjour")))
  }
}