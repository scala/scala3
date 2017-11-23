object MyPhantoms extends Phantom {
  type Inky <: this.Any
  type Blinky <: this.Any
  type Pinky <: Inky
  type Clyde <: Pinky

  unused def pinky: Pinky = assume
  unused def clyde: Clyde = assume
}

import MyPhantoms._
object MyApp {
  def run(unused phantom: Inky) = println("run")
  def hide(unused phantom: Blinky) = println("run")

  run(pinky)
  run(clyde)
}

object MyOtherPhantom extends Phantom {
  type MyPhantom <: this.Any
  unused def myPhantom: MyPhantom = assume

  def f1(a: Int, b: Int)(unused c: MyPhantom): Int = a + b

  def f2 = {
    f1(3, 2)(myPhantom)
  }
}
