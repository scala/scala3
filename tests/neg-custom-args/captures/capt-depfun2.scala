import annotation.retains
class C
type Cap = C @retains[caps.any.type]
class Str

def f(y: Cap, z: Cap) =
  def g(): C @retains[y.type | z.type] = ???
  val ac: ((x: Cap) => Array[Str @retains[x.type]]) = ???
  val dc = ac(g()) // error: Needs explicit type Array[? >: Str <: {y, z} Str]
                   // This is a shortcoming of rechecking since the originally inferred
                   // type is `Array[Str]` and the actual type after rechecking
                   // cannot be expressed as `Array[C Str]` for any capture set C