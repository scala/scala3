import dotty.generic._

case class Foo(i: Int, s: String)

object Test {
  def the[T](implicit ev: T): ev.type = ev
  type &:[H, T[t] <: Prod[t]] = [X] => PCons[[Y] => H, T, X]

  def main(args: Array[String]): Unit = {
    val g = the[Representable[Foo]]
    val a: Representable[Foo] { type Repr = Int &: String &: PNil } = g
  }
}
