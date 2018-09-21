import XmlQuote._

object Test {
  def main(args: Array[String]): Unit = {

    assert(xml"Hello Allan!" == Xml("Hello Allan!", Nil))

    val name = new Object{}
    assert(xml"Hello $name!" == Xml("Hello ??!", List(name)))

    val ctx0 = new StringContext("Hello !")
    assert(ctx0.xml() == Xml("Hello !", Nil))
    assert(new SCOps(ctx0).xml() == Xml("Hello !", Nil))

    val ctx1 = new StringContext("Hello ", "!")
    assert(ctx1.xml(name) == Xml("Hello ??!", List(name)))
    assert(new SCOps(ctx1).xml(name) == Xml("Hello ??!", List(name)))

    val hello: String = "Hello "
    val ctx2 = new StringContext(hello, "!")
    assert(ctx2.xml(name) == Xml("Hello ??!", List(name)))
    assert(new SCOps(ctx2).xml(name) == Xml("Hello ??!", List(name)))

    val args = Seq(name)
    assert(new SCOps(ctx2).xml(args: _*) == Xml("Hello ??!", List(name)))
  }
}
