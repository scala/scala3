import annotation.retains
class C
type Cap = C @retains[caps.cap.type]

type T = (x: Cap) -> String @retains[x.type]

type ID[X] = X

val aa: ((x: Cap) -> String @retains[x.type]) = (x: Cap) => ""

def f(y: Cap, z: Cap): String @retains[caps.cap.type] =
  val a: ((x: Cap) -> String @retains[x.type]) = (x: Cap) => ""
  val b = a(y)
  val c: String @retains[y.type] = b
  def g(): C @retains[y.type | z.type] = ???
  val d = a(g())

  val ac: ((x: Cap) -> ID[String @retains[x.type] -> String @retains[x.type]]) = ???
  val bc: String^{y} -> String^{y} = ac(y)
  val dc: String -> String^{y, z} = ac(g())
  c
