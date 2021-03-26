package example

final type A = Int // error
type A2 = Int

final opaque type B = Int // error
opaque type B2 = Int

final def x = 1 // error
def x2 = 1

final val y = 1 // error
val y2 = 1

final var z = 1 // error
var z2 = 1

final given g: Int = ??? // error
given g2: Int = ???


final class C
final object C // error

class C2
object C2
