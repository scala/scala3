object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun5(boo[Blinky])(15)(boo[Pinky])(16)
    fun5(boo[Inky])(17)(boo[Pinky])(18)
    fun5(boo[Pinky])(19)(boo[Casper])(20)
  }

  def fun5(unused top: Blinky)(n: Int)(unused bottom: Clyde)(n2: Int): Unit = {
    println("fun5")
  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  type Clyde >: Pinky <: Inky
  type Casper = Pinky
  unused def boo[B <: Blinky]: B = assume
}
