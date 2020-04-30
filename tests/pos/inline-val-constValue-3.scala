
inline def f[N <: Int]: Unit = {
  inline val size = compiletime.constValue[N]
  inline val n = size
  val m: Int = n
  ???
}

type N = 4
def test: Unit = f[N]
