class C
type Cap = C @retains(*)

type T = (x: Cap) -> String @retains(x)

val aa: ((x: Cap) -> String @retains(x)) = (x: Cap) => ""

def f(y: Cap, z: Cap): String @retains(*) =
  val a: ((x: Cap) -> String @retains(x)) = (x: Cap) => ""
  val b = a(y)
  val c: String @retains(y) = b
  def g(): C @retains(y, z) = ???
  val d = a(g())

  val ac: ((x: Cap) -> String @retains(x) -> String @retains(x)) = ???
  val bc: (({y} String) -> {y} String) = ac(y)
  val dc: (String -> {y, z} String) = ac(g())
  c
