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

  val a: Blinky = null.asInstanceOf[Blinky] // error
  hide(a)
}
