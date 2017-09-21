object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    new Boo1[BooAny]().polyfun1(boo[Blinky])
    new Boo1[BooAny]().polyfun1(boo[Inky])
  }

  def fun(top: BooAny): Unit = ()

  class Boo1[P <: BooAny] {
    println("Boo1")
    def polyfun1(p1: P): Unit = {
      println("Boo1.polyfun1")
    }
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  type Blinky <: this.Any
  type Inky <: Blinky
  def boo[B <: Blinky]: B = assume
}
