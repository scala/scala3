import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    foo(new Foo(a).aVal)
  }
  def foo(unused a: A) = ()
}

class Foo(unused val aVal: A)

object Boo extends Phantom {
  type A = this.Nothing
  unused def a: A = assume
}
