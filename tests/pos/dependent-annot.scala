class C
class ann(x: Any*) extends annotation.Annotation

def f(y: C, z: C) =
  def g(): C @ann(y, z) = ???
  val ac: ((x: C) => Array[String @ann(x)]) = ???
  val dc = ac(g())
