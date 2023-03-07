class C[T]

type Foo[T] = T match
  case C[true] => true
  case C[false] => false

class W[T] extends C[Foo[T]]

def f[T <: C[?]](t: T) = W[T]()

def test =
  val b = C[true]()
  f(b): C[true]
