class C
type Cap = C @retains(*)

def f(y: Cap, z: Cap) =
  def g(): C @retains(y, z) = ???
  val ac: ((x: Cap) => String @retains(x) => String @retains(x)) = ???
  val dc: (({y, z} String) => {y, z} String) = ac(g()) // error
