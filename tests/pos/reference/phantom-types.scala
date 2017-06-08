object MyPhantoms extends Phantom {
  type Inky <: this.Any
  type Blinky <: this.Any
  type Pinky <: Inky
  type Clyde <: Pinky

  def pinky: Pinky = assume
  def clyde: Clyde = assume
}

import MyPhantoms._
object MyApp {
  def run(phantom: Inky) = println("run")
  def hide(phantom: Blinky) = println("run")

  run(pinky)
  run(clyde)
}

object MyOtherPhantom extends Phantom {
  type MyPhantom <: this.Any
  def myPhantom: MyPhantom = assume

  def f1(a: Int, b: MyPhantom, c: Int): Int = a + c

  def f2 = {
    f1(3, myPhantom, 2)
  }
}
