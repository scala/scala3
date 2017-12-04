import dotty.generic._

sealed trait Foo
case class Bar(i: Int)    extends Foo
case class Bus(s: String) extends Foo
// case class Bip(s: String) extends Foo

object Test {
  def main(args: Array[String]): Unit = {
    // Representable can be synthesised via implicit search
    val g = Representable[Foo]

    // Type is inferred correctly
    val a: Representable[Foo] { type Repr = SCons[Bar, SCons[Bus, SNil]] } = g

    // Representable#to and Representable#from behave as expected:
    assert(g.from(g.to(Bar(1))) == Bar(1))
    assert(g.from(g.to(Bus("s"))) == Bus("s"))
  }
}
