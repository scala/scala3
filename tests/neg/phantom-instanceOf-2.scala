
class phantomInstanceOf2 {
  import Boo._
  boo[Blinky].asInstanceOf[Any] // error
  boo[Blinky].asInstanceOf[Nothing] // error
  boo[Blinky].asInstanceOf[Blinky] // error
  boo[Blinky].asInstanceOf[BooAny] // error
}

object Boo extends Phantom {
  type BooAny <: this.Any
  type Blinky <: this.Any
  def boo[B <: this.Any]: B = assume
}
