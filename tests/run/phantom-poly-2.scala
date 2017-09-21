object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    polyfun2(boo[Blinky])
    polyfun2(boo[Inky])
    polyfun2(boo[Pinky])
  }

  def polyfun2[G <: Blinky](p: G): Unit = {
    println("polyfun2")
  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  def boo[B <: this.Any]: B = assume
}
