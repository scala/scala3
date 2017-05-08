
class Foo {
  import Boo._
  import Boo1._

  type Blinky <: BooAny
  type Inky <: BooAny

  val blinky = Boo.boo[Blinky]
  val inky = Boo.boo[Inky]

  val b = true
  def fooIf1 =
    if (b) blinky
    else ""  // error

  def fooIf2 =
    if (b) ""
    else blinky // error

  def fooIf3 =
    if (b) boo1
    else blinky // error

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
  def fooTry =
    try 1
    catch { case ex: Exception => blinky // error
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
