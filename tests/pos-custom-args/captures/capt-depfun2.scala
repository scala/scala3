import annotation.retains
class C
type Cap = C @retains(caps.cap)

def f(y: Cap, z: Cap) =
  def g(): C @retains(y, z) = ???
  val ac: ((x: Cap) -> Array[String @retains(x)]) = ???
  val dc: Array[? >: String <: String]^{y, z} = ac(g()) // needs to be inferred
  val ec = ac(y)
