super trait S
trait A
class B extends A, S
class C extends A, S

val x = if ??? then B() else C()
val x1: S = x  // error

case object a
case object b
val y = if ??? then a else b
val y1: Product = y      // error
val y2: Serializable = y // error
