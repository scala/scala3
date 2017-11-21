import dotty.generic._

sealed trait Foo
case class Bar(i: Int)    extends Foo
case class Bus(s: String) extends Foo
// case class Bip(s: String) extends Foo

object Test {
  def the[T](implicit ev: T): ev.type = ev
  type |:[H, T[t] <: Sum[t]] = [X] => SCons[[Y] => H, T, X]

  def main(args: Array[String]): Unit = {
    // Representable can be synthesised via implicit search
    val g = the[Representable[Foo]]

    // Type is inferred correctly
    val a: Representable[Foo] { type Repr = Bar |: Bus |: SNil } = g

    // // Representable#to and Representable#from behave as expected:
    // assert(g.from(g.to(Bar(1))) == Bar(1))
    // assert(g.from(g.to(Bus("s"))) == Bus("s"))
  }
}
