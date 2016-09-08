
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky1)(pinky)
    foo1(pinky)
    foo2(new Blinky1)
  }

  def foo1: Pinky => Pinky = new Blinky1
  def foo2(boo: Function1[Pinky, Pinky]) = boo(pinky)
}

class Blinky1 extends Function1[Pinky, Pinky] {
  def apply(p1: Pinky) = {
    println("Blinky1.apply()")
    p1
  }
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
