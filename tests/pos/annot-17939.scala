import scala.annotation.Annotation
class myRefined[T](f: T => Boolean) extends Annotation

class Box[T](val x: T)
class Box2(val x: Int)

class A(a: String @myRefined((x: Int) => Box(3).x == 3)) // crash
class A2(a2: String @myRefined((x: Int) => Box2(3).x == 3)) // works
