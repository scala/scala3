import annotation.retains


class C
type Cap = C @retains(caps.cap)
class Str

def f(y: Cap, z: Cap) =
  def g(): C @retains(y, z) = ???
  val ac: ((x: Cap) => Str @retains(x) => Str @retains(x)) = ???
  val dc: ((Str^{y, z}) => Str^{y, z}) = ac(g()) // error // error: separatioon
