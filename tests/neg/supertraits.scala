transparent trait S
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

enum Color {
  case Red, Green, Blue
}

enum Nucleobase {
  case A, C, G, T
}

val z = if ??? then Color.Red else Nucleobase.G
val z1: reflect.Enum = z // error: Found: (z : Object) Required: reflect.Enum
