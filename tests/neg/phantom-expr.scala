
class Foo {
  import Boo._

  type Blinky <: BooAny
  type Inky <: BooAny

  val blinky = Boo.boo[Blinky]
  val inky = Boo.boo[Inky]

  val b = true
  def fooIf1 = if (b) { blinky } else { "" } // error
  def fooIf2 = if (b) { "" } else { blinky } // error

  def fooMatch1 = blinky match { case _: Blinky => () } // error
  def fooMatch2 = 1 match { case 1 => 2 case _ => blinky } // error
}

object Boo extends Phantom {
  type BooAny = this.Any
  def boo[B <: BooAny]: B = assume[B]
}

