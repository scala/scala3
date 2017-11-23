object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun3(boo[Blinky], boo[Pinky])
    fun3(boo[Inky], boo[Pinky])
    fun3(boo[Pinky], boo[Casper])
  }

  def fun3(unused x1: Blinky, x2: Inky): Unit = {
    println("fun3")
  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  type Casper = Pinky
  unused def boo[B <: Blinky]: B = assume
}
