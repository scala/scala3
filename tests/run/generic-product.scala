import dotty.generic._

case class Foo(i: Int, s: String)
case class Bar()

object Test {
  def main(args: Array[String]): Unit = {
    testFoo()
    testBar()
  }

  def testFoo(): Unit = {
    // Representable can be synthesised via implicit search
    val g = Representable[Foo]

    // Type is inferred correctly
    val a: Representable[Foo] { type Repr = PCons[Int, PCons[String, PNil]] } = g

    // Representable#to and Representable#from behave as expected:
    assert(g.from(g.to(Foo(1, "s"))) == Foo(1, "s"))
  }

  def testBar(): Unit = {
    // Representable can be synthesised via implicit search
    val g = Representable[Bar]

    // Type is inferred correctly
    val a: Representable[Bar] { type Repr = PNil } = g

    // Representable#to and Representable#from behave as expected:
    assert(g.from(g.to(Bar())) == Bar())
  }
}
