import annotation.retains

class C
type Cap = C @retains[caps.any.type]

type T = (x: Cap) -> STR @retains[x.type]

type ID[X] = X

class STR(s: String)

val aa: ((x: Cap) -> STR @retains[x.type]) = (x: Cap) => STR("")

def f(consume y: Cap, z: Cap): STR @retains[caps.any.type] =
  val a: ((x: Cap) -> STR @retains[x.type]) = (x: Cap) => STR("")
  val b = a(y)
  val c: STR @retains[y.type] = b
  def g(): C @retains[y.type | z.type] = ???
  val d = a(g())

  val ac: ((x: Cap) -> ID[STR @retains[x.type] -> STR @retains[x.type]]) = ???
  val bc: STR^{y} -> STR^{y} = ac(y)
  val dc: STR -> STR^{y, z} = ac(g())
  c
