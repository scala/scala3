object Test {
  import M._
  def main(args: Array[String]): Unit = {
    assert(isHello(M.extension_strip("hello")))
    assert(!isHello(M.extension_strip("bonjour")))
  }
}