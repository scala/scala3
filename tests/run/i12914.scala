
class opq:
  opaque type Str = java.lang.String
  object Str:
    def apply(s: String): Str = s
  inline def lower(s: Str): String = s.toLowerCase
  extension (s: Str)
    transparent inline def upper: String = s.toUpperCase
  inline def concat(xs: List[Str]): Str = String(xs.flatten.toArray)
  transparent inline def concat2(xs: List[Str]): Str = String(xs.flatten.toArray)


@main def Test =
  val opq = new opq()
  import opq.*
  val a: Str = Str("aSd")
  println(a.upper)
  println(opq.lower(a))
  def b: Str = Str("aSd")
  println(b.upper)
  println(opq.lower(b))
  def c(): Str = Str("aSd")
  println(c().upper)
  println(opq.lower(c()))
  println(opq.concat(List(a, b, c())))
  println(opq.concat2(List(a, b, c())))

