import caps.*

class C

def test[X^, Y^, Z >: X <: Y](x: C^{X^}, y: C^{Y^}, z: C^{Z^}) =
  val x2z: C^{Z^} = x
  val z2y: C^{Y^} = z
  val x2y: C^{Y^} = x // error
  