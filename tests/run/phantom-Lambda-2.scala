
object Test {
  import Boo._

  def main(args: Array[String]) = {
    val foo = (b: Casper, b2: Casper) => 42
    foo(casper, casper)

    bar1(casper, casper)
    bar2(casper, casper)
  }

  val bar1: (Casper, Casper) => Unit = (b, b1) => println("bar1")
  val bar2 = (b: Casper, b2: Casper) => println("bar2")
}

object Boo extends Phantom {
  type Casper <: Boo.Any
  def casper: Casper = assume
}
