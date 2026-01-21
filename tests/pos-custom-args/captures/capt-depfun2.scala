import annotation.retains

class C
type Cap = C @retains[caps.any.type]

class Arr[T]

class STR
def f(consume y: Cap, consume z: Cap) =
  def g(): C @retains[y.type | z.type] = ???
  val ac: ((x: Cap) => Arr[STR @retains[x.type]]^{x}) = ???
  val dc: Arr[? >: STR <: STR^]^{y, z} = ac(g()) // needs to be inferred
  val ec = ac(y)
