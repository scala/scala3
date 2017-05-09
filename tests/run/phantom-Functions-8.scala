
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    foo3()
  }

  val foo3 = new Blinky()
}

class Blinky extends Boo.Function0[Unit] {
  def apply() = println("Blinky.apply()")
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
