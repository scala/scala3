enum Shape:
  case Square(width: Int, height: Int) extends Shape
  case Circle(radius: Int) extends Shape

@main def Test: Unit =
  println(showType[Shape.Circle])
  println(classOf[Shape.Circle].getName)

