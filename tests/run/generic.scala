import dotty.generic._

case class Foo(i: Int, s: String)

object Test {
  def the[T](implicit ev: T): ev.type = ev
  type &:[H, T[t] <: Prod[t]] = [X] => PCons[[Y] => H, T, X]

  def main(args: Array[String]): Unit = {
    // Representable can be synthesised via implicit search
    val g = the[Representable[Foo]]

    // Type is inferred correctly
    val a: Representable[Foo] { type Repr = Int &: String &: PNil } = g

    // Representable#to and Representable#from behave as expected:
    assert(g.from(g.to(Foo(1, "s"))) == Foo(1, "s"))
  }
}
