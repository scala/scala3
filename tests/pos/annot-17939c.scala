class qualified(f: Int => Boolean) extends annotation.StaticAnnotation
class Box[T](val y: T)
def Test =
  val x: String @qualified((x: Int) => Box(42).y == 2) = ???
  val y = x
