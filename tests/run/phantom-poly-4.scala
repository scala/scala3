object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    polyfun4(boo[Blinky])
    polyfun4(boo[Inky])
    polyfun4(boo[Pinky])
  }

  def polyfun4[P >: BooNothing](p: P): Unit = {
    println("polyfun4")
  }
}

object Boo extends Phantom {
  type BooNothing = Boo.Nothing
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  def boo[B <: Blinky]: B = assume
}
