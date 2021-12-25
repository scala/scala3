class C
type Cap = C @retains(*)

def f(y: Cap, z: Cap) =
  def g(): C @retains(y, z) = ???
  val ac: ((x: Cap) -> Array[String @retains(x)]) = ???
  val dc: Array[? >: String <: {y, z} String] = ac(g()) // needs to be inferred
  val ec = ac(y)
