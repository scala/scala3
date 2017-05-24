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

  hide(pinky) // error
  hide(clyde) // error
  hide(MyPhantoms.assume) // error
  hide(throw new Exception) // error
  hide(???) // error
}
