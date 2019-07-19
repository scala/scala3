sealed trait Color
case object Red extends Color
case object Blue extends Color
case object Green extends Color

class Test {

  val R: Red.type = Red
  val B: Blue.type = Blue
  val G: Green.type = Green

  def go(c: Color): Int = c match {
    case R => 0
    case G => 1
    case B => 2
  }
}