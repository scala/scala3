import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    new Foo(a).aVal
  }
}

class Foo(val aVal: A)

object Boo extends Phantom {
  type A = this.Nothing
  def a = assume
}
