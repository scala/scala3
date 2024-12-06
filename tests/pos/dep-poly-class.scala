trait A:
  type B

class CCPoly[T <: A](a: T, b: a.B)

object Test:
  def test(): Unit =
    val aa: A { type B = Int } = new A { type B = Int }
    val x: CCPoly[aa.type] = CCPoly(aa, 1)
