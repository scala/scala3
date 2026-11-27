import tuplelib.*

case class Elem(name: String)

@main def run(): Unit =
  val tup: (Elem, Elem) = (Elem("a"), Elem("b"))
  val result = tup.mapAs[Elem]()[[_ <: Elem] =>> Boolean](
    [e <: Elem] => (elem: e) => elem.name.nonEmpty
  )
  println(result) // 1
  assert(m() == 1) // 2
  println(annots[L.type]("annotated")) // 3
