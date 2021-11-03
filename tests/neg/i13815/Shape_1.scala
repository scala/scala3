sealed trait Shape

object Shape:
  case class Circle(r: Int) extends Shape
  case class Square(width: Int) extends Shape