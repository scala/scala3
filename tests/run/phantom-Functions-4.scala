
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    foo3(pinky, pinky, pinky, pinky, pinky)
  }

  val foo3: (Pinky, Pinky, Pinky, Pinky, Pinky) => Unit = new Blinky2
}

class Blinky2 extends Blinky

class Blinky extends Function5[Pinky, Pinky, Pinky, Pinky, Pinky, Unit] {
  def apply(p1: Pinky, p2: Pinky, p3: Pinky, p4: Pinky, p5: Pinky) = println("Blinky.apply()")
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
