class C
type Cap = C @retains(*)

def f(y: Cap, z: Cap) =
  def g(): C @retains(y, z) = ???
  val ac: ((x: Cap) => Array[String @retains(x)]) = ???
  val dc = ac(g()) // error: Needs explicit type Array[? >: String <: {y, z} String]
                   // This is a shortcoming of rechecking since the originally inferred
                   // type is `Array[String]` and the actual type after rechecking
                   // cannot be expressed as `Array[C String]` for any capture set C