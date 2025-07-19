import annotation.retains
import caps.consume
class C
type Cap = C @retains[caps.cap.type]

class STR
def f(@consume y: Cap, @consume z: Cap) =
  def g(): C @retains[y.type | z.type] = ???
  val ac: ((x: Cap) -> Array[STR @retains[x.type]]) = ???
  val dc: Array[? >: STR <: STR^]^{y, z} = ac(g()) // needs to be inferred
  val ec = ac(y)
