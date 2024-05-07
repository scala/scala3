class qualified[T](f: T => Boolean) extends annotation.StaticAnnotation

class Box[T](val x: T)
class Box2(val x: Int)

class A(a: String @qualified((x: Int) => Box(3).x == 3)) // crash
class A2(a2: String @qualified((x: Int) => Box2(3).x == 3)) // works
