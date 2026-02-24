import annotation.retains


class C
type Cap = C @retains[caps.any.type]
class Str

def f(y: Cap, z: Cap) =
  def g(): C @retains[y.type | z.type] = ???
  val ac: ((x: Cap) => Str @retains[x.type] => Str @retains[x.type]) = ???
  val dc: ((Str^{y, z}) => Str^{y, z}) = ac(g()) // error
