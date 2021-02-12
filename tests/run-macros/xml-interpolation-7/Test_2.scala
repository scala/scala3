import XmlQuote.*

object Test {
  def main(args: Array[String]): Unit = {
    import XMLOps.xml

    assert(xml"Hello World!" == Xml("Hello World!", Nil))

    val name = new Object{}
    assert(xml"Hello $name!" == Xml("Hello ??!", List(name)))
  }
}
