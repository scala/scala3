class Inv[T]
class Sub[T] extends Inv[T]
def f[S, T](x: S): Sub[S] & Sub[T] = ???
val b: Inv[Int] = f("a")
