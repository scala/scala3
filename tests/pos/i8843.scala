class C:
  type X <: Tuple

inline def f(c: C): Unit = {
  inline val size = compiletime.constValue[Tuple.Size[c.X]]
  val n = size
  val m: Int = n
  ???
}

def test: Unit = f(??? : C { type X = Tuple1[Any] })
