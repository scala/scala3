object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    new Boo4(boo[Blinky])
    new Boo4(boo[Inky])
    new Boo4(boo[Pinky])
  }

  class Boo4(unused p4: Blinky) {
    println("Boo4")
  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  unused def boo[B <: Blinky]: B = assume
}
