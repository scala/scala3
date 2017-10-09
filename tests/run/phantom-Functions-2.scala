
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky)(pinky, pinky)
    (new Blinky).apply(pinky, pinky)

    foo1(pinky, pinky)
    foo2(new Blinky)
  }

  def foo1: (Pinky, Pinky) => Unit = new Blinky
  def foo2(boo: Function2[Pinky, Pinky, Unit]) = boo(pinky, pinky)
}

class Blinky extends Function2[Pinky, Pinky, Unit] {
  def apply(p1: Pinky, p2: Pinky) = println("Blinky.apply()")
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
