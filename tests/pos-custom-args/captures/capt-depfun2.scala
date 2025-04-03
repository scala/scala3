import annotation.retains
class C
type Cap = C @retains[caps.cap.type]

def f(y: Cap, z: Cap) =
  def g(): C @retains[y.type | z.type] = ???
  val ac: ((x: Cap) -> Array[String @retains[x.type]]) = ???
  val dc: Array[? >: String <: String]^{y, z} = ac(g()) // needs to be inferred
  val ec = ac(y)
