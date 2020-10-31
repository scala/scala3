object Test {
  import M._
  def main(args: Array[String]): Unit = {
    assert(isHello(M.strip("hello")))
    assert(!isHello(M.strip("bonjour")))
  }
}