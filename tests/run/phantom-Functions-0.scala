
import Boo._

object Test {

  def main(args: Array[String]): Unit = {
    (new Blinky1)(pinky)
    foo1(pinky)
    foo2(new Blinky1)
  }

  def foo1: Pinky => Unit = new Blinky1
  def foo2(fun3: Function1[Pinky, Unit]) = fun3(pinky)
}

class Blinky1 extends Function1[Pinky, Unit] {
  def apply(p1: Pinky) = println("Blinky1.apply()")
}

object Boo extends Phantom {
  type Pinky <: this.Any
  def pinky: Pinky = assume
}
