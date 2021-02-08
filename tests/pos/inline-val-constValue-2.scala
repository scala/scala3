import compiletime.*

class C:
  type N <: Int

def test: Unit =
  val a: C { type N = 3 } = ???
  f(a)

inline def f(c: C): Unit = {
  inline val size = constValue[c.N]
  val n = size
  val m: Int = n
  ???
}
