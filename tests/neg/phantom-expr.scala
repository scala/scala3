
class Foo {
  import Boo._
  import Boo1._

  type Blinky <: BooAny
  type Inky <: BooAny

  val blinky = Boo.boo[Blinky]
  val inky = Boo.boo[Inky]

  val b = true
  def fooIf1 =
    if (b) blinky // error
    else ""

  def fooIf2 =
    if (b) "" // error
    else blinky

  def fooIf3 =
    if (b) boo1 // error
    else blinky

  def fooMatch1 = blinky match { // error
    case _: Blinky => ()
  }
  def fooMatch2 = 1 match { case 1 => 2
    case _ => blinky // error
  }
  def fooMatch3 = 1 match {
    case 1 => boo1
    case _ => blinky // error
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def boo[B <: BooAny]: B = assume
}

object Boo1 extends Phantom {
  type Boo1Any = this.Any
  def boo1: Boo1Any = assume
}