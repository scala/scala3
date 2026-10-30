class C[-X]
class Inv[T]
def outer[T](x: C[Inv[Int] & T]): Inv[T] = ???
def middle[X](x: C[X]): C[X] = ???
def inner[U](): C[Inv[U] & U] = ???

val a = outer(middle(inner()))
val b: Inv[Int] = a
