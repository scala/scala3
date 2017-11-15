import dotty.generic._

case class Foo(i: Int, s: String)

object Test {
  def main(args: Array[String]): Unit = {
    val foo = Foo(1, "s")
    val g = implicitly[Representable[Foo]]
    assert(g.to(foo) == PCons(1, PCons("s", PNil)))
  }
}
