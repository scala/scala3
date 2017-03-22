
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky)(42, pinky)
    (new Blinky).apply(43, pinky)

    foo1(44, pinky)
    foo2(new Blinky)
  }

  def foo1: (Int, Pinky) => Unit = new Blinky
  def foo2(boo: Function2[Int, Pinky, Unit]) = boo(47, pinky)
}

class Blinky extends Function2[Int, Pinky, Unit] {
  def apply(p1: Int, p2: Pinky) = println("Blinky.apply(" + p1 + ")")
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
