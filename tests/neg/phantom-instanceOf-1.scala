
class phantomInstanceOf1 {
  null.asInstanceOf[Boo.Any] // error
  null.asInstanceOf[Boo.Nothing] // error
  "".asInstanceOf[Boo.Any] // error
  "".asInstanceOf[Boo.Nothing] // error
}

object Boo extends Phantom {
  def boo[B <: Boo.Any]: B = assume
}
