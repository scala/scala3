class Both[+A, -B]
class Inv[T]
def outer[T, U](x: Both[Inv[U], Inv[Int] & T]): Inv[T] = ???
def middle[X](f: X => Int): Both[X, X] = ???
val a = outer(middle(x => { val y: Inv[Int] = x; 1 }))
