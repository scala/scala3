transparent trait S
trait A
class B extends A, S
class C extends A, S

val x = if ??? then B() else C()
val x1: S = x  // error

class Top
case object a extends Top
case object b extends Top
val y = if ??? then a else b
val y1: Product = y      // error
val y2: Serializable = y // error

enum Color extends Top {
  case Red, Green, Blue
}

enum Nucleobase extends Top {
  case A, C, G, T
}

val z = if ??? then Color.Red else Nucleobase.G
val z1: reflect.Enum = z // error: Found: (z : Top) Required: reflect.Enum
