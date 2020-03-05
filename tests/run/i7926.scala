trait Foo {
  def parent: Option[Foo]
  def getParent: Option[Foo] = parent
}

class Bar(val parent: Option[Foo]) extends Foo
class Qux(parent: Option[Qux]) extends Bar(parent)

object Test {
  val qux: Qux = new Qux(None)

  def main(args: Array[String]): Unit = println(qux.getParent)
}