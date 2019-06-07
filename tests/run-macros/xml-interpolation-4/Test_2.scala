import XmlQuote._

object Test {
  def main(args: Array[String]): Unit = {
    assert(xml"Hello ${implicitly[Scope]} world!" == "Hello Scope(top+) world!")
    assert(xml"Hello ${ xml"world ${implicitly[Scope] + " " + xml"${implicitly[Scope]}"}" }!" ==
        "Hello world Scope(top++) Scope(top+++)!")
  }
}
