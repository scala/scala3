
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    foo3(44, pinky, 0.4, pinky, pinky)
  }

  val foo3: (Int, Pinky, Double, Pinky, Pinky) => Unit = new Blinky2().asInstanceOf[Function5[Int, Pinky, Double, Pinky, Pinky, Unit]]
}

class Blinky2 extends Blinky

trait Blinky extends Function5[Int, Pinky, Double, Pinky, Pinky, Unit] {
  def apply(p1: Int, p2: Pinky, p3: Double, p4: Pinky, p5: Pinky) = println("Blinky.apply(" + p1 + ")")
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
