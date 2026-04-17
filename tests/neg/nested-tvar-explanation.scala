trait Ord[T]
trait Op[A, V]
object Op:
  given Op[Int, Int] = new Op[Int, Int] {}

def min[A, V, W <: Ord[V]](x: A)(implicit op: Op[A, V], w: W): W = w
def foo(x: Double) = ???
def test = foo(min(3)) // error
