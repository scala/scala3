import dotty.Show._

class Foo(x: Int)

object Test {
  val res0 = new Foo("Hello") // error
}
