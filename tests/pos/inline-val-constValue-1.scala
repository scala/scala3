import compiletime.*

class C:
  type X <: Tuple

def test: Unit =
  val a: C { type X = Tuple1[Any] } = ???
  f(a)

inline def f(c: C): Unit = {
  inline val size = constValue[Tuple.Size[c.X]]
  val n = size
  val m: Int = n
  ???
}
