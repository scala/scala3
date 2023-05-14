import annotation.retains
class C
type Cap = C @retains(caps.cap)

type T = (x: Cap) -> String @retains(x)

type ID[X] = X

val aa: ((x: Cap) -> String @retains(x)) = (x: Cap) => ""

def f(y: Cap, z: Cap): String @retains(caps.cap) =
  val a: ((x: Cap) -> String @retains(x)) = (x: Cap) => ""
  val b = a(y)
  val c: String @retains(y) = b
  def g(): C @retains(y, z) = ???
  val d = a(g())

  val ac: ((x: Cap) -> ID[String @retains(x) -> String @retains(x)]) = ???
  val bc: String^{y} -> String^{y} = ac(y)
  val dc: String -> String^{y, z} = ac(g())
  c
