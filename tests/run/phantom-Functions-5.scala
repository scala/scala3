
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    foo3(44, pinky, 0.4, pinky, pinky)
  }

  val foo3: (Int, Pinky, Double, Pinky, Pinky) => Unit = new Blinky2
}

class Blinky2 extends Blinky {
  def apply(p1: Int, p2: Pinky, p3: Double, p4: Pinky, p5: Pinky) = println("Blinky2.apply(" + p1 + ")")
}

abstract class Blinky extends Function5[Int, Pinky, Double, Pinky, Pinky, Unit]

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
