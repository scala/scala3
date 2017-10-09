
object Test {
  import Boo._

  def main(args: Array[String]) = {
    foo1(casper)
    foo2(casper)
  }

  def foo1: Casper => Unit = b => println("foo1")
  def foo2 = (b: Casper) => println("foo2")
}

object Boo extends Phantom {
  type Casper <: Boo.Any
  def casper: Casper = assume
}
