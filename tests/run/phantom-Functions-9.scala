
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky1)(42)
    foo1(42)
    foo2(new Blinky1)
  }

  def foo1: Int => Pinky = new Blinky1
  def foo2(boo: Function1[Int, Pinky]) = boo(42)
}

class Blinky1 extends Function1[Int, Pinky] {
  def apply(i: Int) = {
    println("Blinky1.apply()")
    pinky
  }
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
